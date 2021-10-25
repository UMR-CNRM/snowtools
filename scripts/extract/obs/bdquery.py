# -*- coding: utf-8 -*-

import psycopg2
from snowtools.utils.dates import check_and_convert_date


class question(object):

    """
    Class for interrogating PostGreSQL database

    Useful for accessing BDClim at Meteo-France. Please consider using directly
    ``psycopg2`` or ``pandas`` for new projects and make sure you are familiar
    with SQL syntax before using as inputs are not always correctly sanitized.
    """

    def __init__(self, host='bdclimop-usr.meteo.fr', port=5432, user='anonymous', password='anonymous',
                 database='bdclim', listvar=[], listorder=[], listconditions=[], listjoin=[], period=[],
                 table=None, dateformat='iso'):
        """
        :param host: Database server hostname
        :type host: str
        :param port: Database server port
        :type: int
        :param user: Database username
        :type: str
        :param password: Database password
        :type: str
        :param database: Database name to use on the server
        :type: str
        :param listvar: List of variables to read
        :type listvar: list of str
        :param listorder: List of variables to order on
        :type listorder: list of str
        :param listconditions: List of conditions to apply for data selection (SQL format)
        :type: list of str
        :param listjoin: List of join to be done (SQL format without 'JOIN')
        :type join: list of str
        :param period: List [[Min date, [max_date]]] for filtering on dates
                       (field dat, else, put it in conditions please)
        :type period: list of str or datetime
        :param table: The table to read in the selected database
        :param dateformat: Date format (iso, daily or monthly)
        :type dateformat: str
        :type table: str
        """
        if listconditions is None:
            listconditions = []
        self.host = host
        self.port = port
        self.user = user
        self.password = password
        self.database = database
        self.variables = listvar
        self.order = listorder
        self.conditions = listconditions
        self.join = listjoin
        self.period = period
        self.table = table
        self.dateformat = dateformat
        self.sql = None

    def set_varout(self, listvar):
        """
        Set the list of desired output variables
        """
        self.variables = listvar

    def set_order(self, listvar):
        """
        Set the list of sorting variables
        """
        if isinstance(listvar, list):
            self.order = listvar
        else:
            self.order.append(listvar)

    def set_table(self, table):
        """
        Define the table
        """
        self.table = table

    def set_period(self, datedeb, datefin):
        """
        Define the period with begin and end date

        :param datedeb: Begin date
        :type datedeb: str or datetime object
        :param datefin: End date
        :type datefin: str or datetime object
        """
        self.period = [datedeb, datefin]

    def set_condition(self, condition):
        """
        Set a condition, written as a SQL condition
        """
        self.conditions.append(condition)

    def set_join(self, join_str):
        """
        Set a join of table, written as a SQL JOIN.
        Example: ``text ON text.id = id_text``
        """
        self.join.append(join_str)

    def _format_date(self, date):
        if self.dateformat == 'iso':
            return date.isoformat(sep=' ')
        elif self.dateformat == 'daily':
            return date.strftime('%Y%m%d')
        elif self.dateformat == 'monthly':
            return date.strftime('%Y%m')
        else:
            raise ValueError('Date format must be one of iso, daily, monthly')

    def get(self, outputfile=None, sep=';', header=True, mode='w'):
        """
        Run the query to the database and return an array of results

        :param outputfile: Output csv file in which to write the results.
                           If set to None, an array of results is returned
        :type outputfile: str or path-like
        :param sep: Separator in case of csv output is specified
        :type sep: one-character str
        :param header: Headers in case of CSV file. Defaults to True (put the headers),
                       can also be False to desactivate headers or a list to override
                       default ones
        :type header: bool or list
        :param mode: Mode for opening outputfile
        :type mode: str
        """
        # construction of SQL request
        self.sql = 'SELECT {fields} FROM {table}'.format(fields=','.join(self.variables), table=self.table)
        if len(self.join) > 0:
            self.sql += ' JOIN {}'.format('JOIN '.join(self.join))
        if len(self.conditions) > 0 or len(self.period) > 0:
            conditions = self.conditions
            if len(self.period) > 0:
                if isinstance(self.period[0], str):
                    date = check_and_convert_date(self.period[0])
                else:
                    date = self.period[0]
                date_formatted = self._format_date(date)
                conditions += ['DAT >= \'{}\''.format(date_formatted)]
            if len(self.period) > 1:
                if isinstance(self.period[1], str):
                    date = check_and_convert_date(self.period[1])
                else:
                    date = self.period[1]
                date_formatted = self._format_date(date)
                conditions += ['DAT <= \'{}\''.format(date_formatted)]
            self.sql += ' WHERE {}'.format(' AND '.join(self.conditions))
        if len(self.order) > 0:
            self.sql += ' ORDER BY {}'.format(','.join(self.order))

        # Connexion to database
        with psycopg2.connect(
                host=self.host,
                user=self.user,
                password=self.password,
                port=self.port,
                database=self.database
                ) as conn:
            cursor = conn.cursor()
            cursor.execute(self.sql)

            if outputfile is None:
                data = cursor.fetchall()
                return data
            else:
                import csv
                with open(outputfile, mode, newline='') as f:
                    writer = csv.writer(f, delimiter=sep)
                    if header is True:
                        writer.writerow(self.variables)
                    elif header is not False:
                        writer.writerow(header)
                    writer.writerows(cursor)

    def run(self, *args, **kwargs):
        """
        For backward compatibility only. Please do not use.

        meta:private
        """
        return self.get(*args, **kwargs)
