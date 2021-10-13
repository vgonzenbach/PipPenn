"""Combine med taking session for iterative training"""
from itertools import combinations
import argparse

parser = argparse.ArgumentParser()
parser.add_argument('Size', metavar='r', type=str, help='the size of each combination')
args = parser.parse_args()
r = args.Size

med_sessions = [5,6,8,9,11,12]
if int(r) == 1:
    for ses in med_sessions:
        print(ses)
elif int(r) > 0:
    combs = [str(i) for i in combinations(med_sessions, int(r))]
    lines = [line.replace('(', '').replace(')', '').replace(' ', '') for line in combs]
    for line in lines:
        print(line)
