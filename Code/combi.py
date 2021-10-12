"""Combine med taking session for iterative training"""
from itertools import combinations
import argparse

parser = argparse.ArgumentParser()
parser.add_argument('Size', metavar='r', type=str, help='the size of each combination')
args = parser.parse_args()
r = args.Size

combs = [str(i) for i in combinations([5,6,8,9,11,12], int(r))]
lines = [line.replace('(', '').replace(')', '').replace(' ', '') for line in combs]
for line in lines:
    print(line)