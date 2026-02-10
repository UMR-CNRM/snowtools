import argparse
import glob
import tarfile
import warnings

import numpy as np
import pandas as pd
import xarray as xr
from bronx.stdtypes.date import Date, Period
from pandas.errors import EmptyDataError

warnings.filterwarnings("ignore")


def write_fixed_width_file(df, col_widths, rep_name):
    """
    Write DataFrame dans un fichier avec col largeur fixe.

    Args:
        df (pd.DataFrame): Le DataFrame
        col_widths (dict): Dictionnaire {colonne: largeur}.
        rep_name (str): Nom du fichier de sortie.
    """
    with open(rep_name, "w", encoding="utf-8") as f:
        for _, row in df.iterrows():
            line = ""
            for col in df.columns:
                val = row[col]
                width = col_widths[col]

                # Formatage selon le type de valeur
                if isinstance(val, str):
                    val_formatted = f"{val:<{width}}"
                elif isinstance(val, int):
                    val_formatted = f"{val:>{width}d}"
                elif isinstance(val, float):
                    val_formatted = f"{val:>{width}.2f}"
                else:
                    val_formatted = f"{str(val):<{width}}"

                # Troncature si nécessaire
                val_formatted = val_formatted[:width]

                line += val_formatted
            f.write(line + "\n")


def replace_obs_tar(tar_name):
    # Load datasets
    reconstructed_data = xr.open_dataset("NEW_OBSERVATIONS.nc")
    listeo = pd.read_csv("listeo_reanalyse", header=None)

    intersect = list(set(np.unique(listeo[24].values)) & set(reconstructed_data.num_poste.values))
    ## Sel common stations in both reconstructed and listeo
    listeo = listeo[listeo[24].isin(intersect)]
    listeo = listeo[[24, 26]].rename(columns={24: "num_poste", 26: "massif_number"}).set_index("num_poste").to_xarray()

    reconstructed_data = reconstructed_data.sel(num_poste=intersect)
    ## add massif number to obs rec
    reconstructed_data = reconstructed_data.assign_coords(listeo)

    ls_file_type = ["S", "T"]

    for file_type in ls_file_type:
        for obs_file in glob.glob(f"{file_type}????????"):
            date = obs_file[1:]

            # Define century (here because of 1999-2000)
            if int(date[0:2]) < 30:
                century = "20"
            else:
                century = "19"

            date = Date(century + date)
            print(date)

            # Load and read correctly obs file
            column_widths = [6, 6, 6, 10, 10, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 1, 8, 1, 1, 1, 3, 1, 3, 1, 3, 1, 2, 1, 2, 1, 6, 1, 1, 1, 1, 3]
            try:
                df = pd.read_fwf(obs_file, header=None, widths=column_widths, converters={18: str})
            except EmptyDataError:
                print(f"Empty file : {obs_file}")
                df = pd.DataFrame(columns=range(len(column_widths)))

            df = df.rename(columns={0: "type_poste_actuel", 1: "lat", 2: "lon", 3: "Station_Name", 4: "num_poste", 6: "Temperature", 9: "ZS", 18: "time"})

            # Transform and recup only reconstituted ds at the time
            # Select right date in reconstituted obs
            if file_type == "S":
                # When "date" is missing, sel(time=date) crashes while selecting a slice returns
                reconstituted_obs_i = reconstructed_data.sel(time=slice(date, date))
                timestep = [date.strftime("%y%m%d%H")]
            elif file_type == "T":
                begin = (date - Period(hours=23)).strftime("%Y%m%dT%H")
                end = date.strftime("%Y%m%dT%H")
                # Transform it in the right format
                reconstituted_obs_i = reconstructed_data.sel(time=slice(begin, end))
                timestep = [pd.to_datetime(a_date).strftime("%y%m%d%H") for a_date in reconstituted_obs_i["time"].values]

            if len(reconstituted_obs_i.time) > 0:
                reconstituted_obs_i["time"] = timestep
                reconstituted_obs_i["t"] = (reconstituted_obs_i["t"] * 100).fillna(999999).astype(int)
                reconstituted_obs_i["ZS"] = (reconstituted_obs_i["ZS"] * 10).fillna(999999).astype(int)
                reconstituted_obs_i = reconstituted_obs_i.set_coords("type_poste_actuel")
                reconstituted_obs_i["Station_Name"] = reconstituted_obs_i["Station_Name"].astype("<U10")

                # Subselect OBS which are already present in OBS files
                intersect = list(set(np.unique(df["num_poste"]).data) & set(reconstituted_obs_i.num_poste.values))
                obs_present = reconstituted_obs_i.sel(num_poste=intersect)["t"]
                obs_present = obs_present.T.to_series().fillna(99999)

                # Replace df by the obs present if not present at that time at first (prioritize real obs)
                df_replace = df.set_index(["num_poste", "time"])
                df_replace["Temperature"] = obs_present.combine_first(df_replace["Temperature"])
                df_replace["type_poste_actuel"] = df_replace["type_poste_actuel"].astype("Int64").fillna(9)

                # Added station
                added_station = list(set(intersect).symmetric_difference(reconstituted_obs_i.num_poste.values))
                obs_added = reconstituted_obs_i.sel(num_poste=added_station)["t"].T.to_dataframe()
                if file_type == "S":
                    obs_added = obs_added.reset_index().set_index(["num_poste", "time"])
                obs_added = obs_added[["t", "type_poste_actuel", "Station_Name", "lat", "lon", "ZS", "massif_number"]].rename(columns={"t": "Temperature"})
                obs_added["lat"] = (obs_added["lat"] * 100).astype(int)
                obs_added["lon"] = (obs_added["lon"] * 100).astype(int)
                obs_added["type_poste_actuel"] = obs_added["type_poste_actuel"].fillna(9)
                obs_added = obs_added.fillna(999999)

                # Complete obs with new obs and sort them by first those : already in the file obs / inside massif / outside massif
                df_replace = pd.concat([df_replace, obs_added.sort_values("massif_number", ascending=False)], sort=False).reset_index(level=["num_poste", "time"])

                # Fill missing values or empty character by space
                list_columns = [17, 19, 20, 21, 23, 25, 27, 29, 31, 33, 34, 35, 36]
                df_replace[list_columns] = df_replace[list_columns].fillna("")

                # Fill all missing values for int by 999999
                df_replace = df_replace.set_index(["Station_Name", "time", "num_poste"] + list_columns)
                df_replace = df_replace.astype("Int64").fillna(999999)
                df_replace = df_replace.reset_index()

                # Reorder df as beginning
                df_replace = df_replace[df.columns.values]
                # Écriture dans un fichier
                col_widths = dict(zip(df_replace.columns.values, column_widths))
                write_fixed_width_file(df_replace, col_widths, obs_file)

    # Création de la nouvelle archive
    tar = tarfile.open(tar_name, "w")
    tarfiles = glob.glob("[RSTA]????????")
    for file in tarfiles:
        tar.add(file)
    tar.close()


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-f", "--tar_name", help="tar file to treat")

    options = parser.parse_args()

    replace_obs_tar(options.tar_name)
