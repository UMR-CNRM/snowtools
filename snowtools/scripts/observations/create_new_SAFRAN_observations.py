import argparse
import json
import os
import pickle
import tarfile
import warnings

import numpy as np
import pandas as pd
import xarray as xr
from bronx.stdtypes.date import Date, Period

warnings.filterwarnings("ignore")

parser = argparse.ArgumentParser()
parser.add_argument("-f", "--tar_name", help="tar file to treat")

options = parser.parse_args()
print(options.tar_name)


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
    # Opening of reconstituted observations and metadata
    metadata = xr.load_dataset("bdclim_SAFRAN_t_table_H_1950-2024_metadata.nc").drop("time")
    # TODO : à faire dépendre de datebegin / dateend
    timeline = pd.date_range(start="1/1/1950", end="1/1/2024", freq="1H")[0:-1]
    # Load datasets
    with open("reconstructed_data_GRIN.pkl", "rb") as f:
        reconstructed_data = pickle.load(f)
    reconstructed_data = np.array(reconstructed_data)

    # Predictors
    with open("predictors.json", "r") as f:
        predictors = json.load(f)
    predictors = pd.read_json(predictors).to_xarray()
    predictors = predictors.assign_coords(station_count=np.arange(predictors.sizes["index"]))

    # Transform back into dataarray
    reconstructed_data = xr.DataArray(
        reconstructed_data,
        dims=["time", "station_count"],
        coords={"time": timeline, "station_count": np.arange(reconstructed_data.shape[1])},
    )

    # Associate num_poste
    reconstructed_data["station_count"] = predictors["index"].values
    reconstructed_data = reconstructed_data.rename({"station_count": "num_poste"})

    # Intersection of reconstructed and original datasets
    intersect_poste = list(set(metadata.num_poste.values) & set(reconstructed_data.num_poste.values))

    reconstructed_data = reconstructed_data.sel(num_poste=intersect_poste)
    metadata = metadata.sel(num_poste=intersect_poste)

    # Associate metadata
    reconstructed_data = reconstructed_data.assign_coords(
        {
            "region": metadata.region.astype(str),
            "type_poste_actuel": metadata.type_poste_actuel.astype(int),
            "lon": metadata.lon,
            "lat": metadata.lat,
            "reseau_poste_actuel": metadata.reseau_poste_actuel.astype(int),
            "ZS": metadata.ZS,
            "Station_Name": metadata.Station_Name.astype(str),
        }
    )

    reconstructed_data["station_count"] = ("num_poste", np.arange(0, len(reconstructed_data.num_poste), 1))
    reconstructed_data = reconstructed_data.to_dataset(name="t")

    ls_file_type = ["S", "T"]
    ls_file = {}

    # Name of the file without .tar
    rep_name = os.path.splitext(tar_name)[0]

    # Open and extract tarfile and list all T and S files
    with tarfile.open(tar_name, "r:") as tar:
        os.makedirs(rep_name, exist_ok=True)
        # Extract archive
        tar.extractall(rep_name)

        for file_type in ls_file_type:
            # Get rep_name to be modified : either T or S files
            ls_file[file_type] = tar.getnames()
            # Only T and S starting file
            ls_file[file_type] = [n for n in ls_file[file_type] if n.startswith(file_type)]
            ls_file[file_type] = [str(i)[1:] for i in ls_file[file_type]]

    for file_type in ls_file_type:
        for date in ls_file[file_type]:
            print(date)
            # Define century (here because of 1999-2000)
            if int(date[0:2]) < 30:
                century = "20"
            else:
                century = "19"

            # Load and read correctly obs file
            column_widths = [6, 6, 6, 10, 10, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 1, 8, 1, 1, 1, 3, 1, 3, 1, 3, 1, 2, 1, 2, 1, 6, 1, 1, 1, 1, 3]
            df = pd.read_fwf(rep_name + "/" + file_type + date, header=None, widths=column_widths, converters={18: str})
            df = df.rename(columns={0: "type_poste_actuel", 1: "lat", 2: "lon", 3: "Station_Name", 4: "num_poste", 6: "Temperature", 9: "ZS", 18: "time"})

            # Transform and recup only reconstituted ds at the time
            # Select right date in reconstituted obs
            if file_type == "S":
                begin = pd.to_datetime(century + date[0:2] + date[2:6] + "T" + date[6:8]).strftime("%Y%m%dT%H")
                # Transform it in the right format
                reconstituted_obs_i = reconstructed_data.sel(time=begin)
                timestep = [pd.to_datetime(date).strftime("%Y%m%d%H")[2:] for date in [reconstituted_obs_i["time"].values]]
            elif file_type == "T":
                begin = (Date(century + date[0:2] + date[2:6] + "T07") - Period(days=1)).strftime("%Y%m%dT%H")
                end = pd.to_datetime(century + date[0:2] + date[2:6] + "T06").strftime("%Y%m%dT%H")
                # Transform it in the right format
                reconstituted_obs_i = reconstructed_data.sel(time=slice(begin, end))
                timestep = [Date(pd.to_datetime(date)).ymdh[2:] for date in reconstituted_obs_i["time"].values]

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
            df["time"] = [Date(pd.to_datetime(date, format="%y%m%d%H")).ymdh[2:] for date in df["time"].values]
            df_replace = df.set_index(["num_poste", "time"])
            # df_replace.index = df_replace.index.set_levels([df_replace.index.levels[0],
            #    pd.to_datetime(df_replace.index.levels[1], format='%y%m%d%H')])
            df_replace["Temperature"] = obs_present.combine_first(df_replace["Temperature"])
            df_replace["type_poste_actuel"] = df_replace["type_poste_actuel"].astype("Int64").fillna(9)

            # Added station
            added_station = list(set(intersect).symmetric_difference(reconstituted_obs_i.num_poste.values))
            obs_added = reconstituted_obs_i.sel(num_poste=added_station)["t"].T.to_dataframe()
            if file_type == "S":
                obs_added["time"] = date
                obs_added = obs_added.reset_index().set_index(["num_poste", "time"])
            obs_added = obs_added[["t", "type_poste_actuel", "Station_Name", "lat", "lon", "ZS"]].rename(columns={"t": "Temperature"})
            obs_added["lat"] = (obs_added["lat"] * 100).astype(int)
            obs_added["lon"] = (obs_added["lon"] * 100).astype(int)
            obs_added["type_poste_actuel"] = obs_added["type_poste_actuel"].fillna(9)
            obs_added = obs_added.fillna(999999)

            # Complete obs
            df_replace = pd.concat([df_replace, obs_added]).reset_index(level=["num_poste", "time"])

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
            write_fixed_width_file(df_replace, col_widths, rep_name + "/" + file_type + date)

    # Création de la nouvelle archive
    new_tar_name = tar_name[0:6] + ".tar"
    tar = tarfile.open(new_tar_name, "w")
    tarfiles = os.listdir(rep_name)
    os.chdir(rep_name)
    for file in tarfiles:
        tar.add(file)
    tar.close()


if __name__ == "__main__":
    replace_obs_tar(options.tar_name)
