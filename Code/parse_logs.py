import os 
from glob import glob
import re
import pandas as pd

def setwd():
    WORKDIR = os.path.join(os.path.dirname(__file__),'..')
    os.chdir(WORKDIR)
    return None

def parse_log(log_file):
    """Parse a log file for all jobs and return parameters for each. Returns dataframe"""
    with open(log_file, 'r') as f:
        text = f.read()

    logs = re.split("Sender: LSF System", text)[1:]
    def get_params(log):
        patient = re.search("(\d+).rds", log).group(1)
        hand = re.search("--hand\s+(\w+)", log).group(1)
        ses = re.search("--ses\s([0-9,]+)", log).group(1)
        npc = re.search("--npc\s+(\d+)", log).group(1)
        # extract elapsed time
        times = re.search("Running prediction...\s+user\s+system\s+elapsed\s+(.*)", log).group(1)
        elapsed = re.split("\s+", times)[2] 
        return {'patient': patient, 'hand': hand, 'ses': ses, 'npc': npc, 'elapsed': elapsed}

    # filter by complete job, i.e. Prediction saved in log
    params = [get_params(log) for log in filter(lambda x: "\nPrediction saved.\n" in x, logs)]
    return pd.DataFrame.from_records(params)
    
if __name__ == "__main__":
    setwd()
    log_files = glob("logs/bothhands/npc-*_smooth-TRUE.log") 
    runtime_df = pd.concat([parse_log(log_file) for log_file in log_files]).reset_index(drop=True)
    runtime_df.to_csv('runtime_df.csv', index=False)

