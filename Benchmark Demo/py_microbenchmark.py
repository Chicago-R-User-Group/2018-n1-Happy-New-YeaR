#!/usr/bin/env python3
import collections as col
import numpy as np
import time as tt
import tabulate as tab
from CHI_CommAreas_Census2010_pandas import extract_fct, reshape_fct, summarize_fct, graphing_fct

np.set_printoptions(suppress=True)

class SimpleTable(col.OrderedDict):
    """
        SimpleTable is just a OrderedDict with some convenience functions
        It is not designed to be full featured nor does it do any checking
        of the table structure.
        The keys represent the column headers and the values represent the
        columns.
    """
                
    def viewkeys(self):
        """
        Returns the keys of ordered dictionary
        """
        return self.keys()
    
    def nrow(self):
        """
        Returns the number of rows in the table
        """
        return self[list(self.viewkeys())[0]].__len__()
    
    def ncol(self):
        """
        Returns the number of columns in the table
        """
        return self.__len__()
    
    def rbind(self, row):
        """
        Binds any suitable dict, OrderedDict, or SimpleTable
        To the current table
        :row: suitable dict or SimpleTable of rows to be bound
        """
        for k in self.viewkeys():
            for i in np.arange(row[k].__len__()):
                self[k].append(row[k][i])
                
    def __repr__(self):
        return tab.tabulate(self, headers = "keys")

# Function to transform and round time units
def timeUnits(times, unit = "ms", ndigits = 5):
    """
    Function formats times for the bench function
    :param times: list of times to be formatted
    :param unit: string the units of time: "ms": (milliseconds), "us": (microseconds),
                    "ns": nanoseconds, "s": time in seconds, "raw": raw time in seconds
    :param ndigits: number of decimal places to round down the times
    :return: list of formatted times
    """
    if unit == "ms":
        return [round(i * 1E3, ndigits) for i in times]
    elif unit == "us":
        return [round(i * 1E6, ndigits) for i in times]
    elif unit == "ns":
        return [round(i * 1E9, ndigits) for i in times]
    elif unit == "s":
        return [round(i, ndigits) for i in times]
    elif unit == "raw":
        return times

# Create a function to do the benchmarking
def bench(sExpr, neval=1, units = "ms", ndigits = 5):
    """
    :param expr: string expression to be evaluated
    :param neval: number of evaluations that the statistics will be calculated from
    :param units: string the units of time: "ms": (milliseconds), "us": (microseconds),
                    "ns": nanoseconds, "s": time in seconds, "raw": raw time in seconds
    :param ndigits: number of decimal places to round down the times
    :return: SimpleTable of times min, lower, mid, upper quartiles and max time, and the expression run
    """
    times = np.ndarray(shape=(neval,), dtype = np.float64)
    expr = compile(sExpr, "<string>", "eval")
    for i in np.arange(neval):
        start = tt.time()
        out = eval(expr)
        end = tt.time()
        times[i] = end - start
    times = np.percentile(times, [0, 25, 50, 75, 100])
    times = timeUnits(times, units, ndigits)
    summ = SimpleTable([('expr', [sExpr]), ('min', [times[0]]), ('lq', [times[1]]), ('median', [times[2]]),
                        ('uq', [times[3]]), ('max', [times[4]]), ('neval', [neval])])
    
    return summ


def benchmark(*args, **kwargs):
    """
    Function running bench underneath for multiple strings
    :param args: the strings to be evaluated
    :param kwargs: arguments passed to bench
    :return: SimpleTable of times min, lower, mid, upper quartiles and max time, and the expression run
    """
    nExpr = args.__len__()
    out = bench(args[0], **kwargs)
    if nExpr > 1:
        for i in np.arange(1, nExpr):
            out.rbind(bench(args[i], **kwargs))
    return out


def lbenchmark(lExpr, **kwargs):
    """
    List version of benchmark, takes in a list of string expressions as lExpr
    :param lExpr: list of strings to be evaluated
    :param neval: number of evaluations that the statistics will be calculated from
    :param units: string the units of time: "ms": (milliseconds), "us": (microseconds),
                    "ns": nanoseconds, "s": time in seconds, "raw": raw time in seconds
    :param ndigits: number of decimal places to round down the times
    :return: SimpleTable of times min, lower, mid, upper quartiles and max time, and the expression run
    """
    nExpr = lExpr.__len__()
    out = bench(lExpr[0], **kwargs)
    if nExpr > 1:
        for i in np.arange(1, nExpr):
            out.rbind(bench(lExpr[i], **kwargs))
    return out

