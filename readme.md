# Model Well Math

### Introduction

### Usage

```
mod_wel_math.exe config_file
```
`config_file` is the configuration file following the format below:
```
# comment lines start with the '#' sign.
ZONE_FILE_NAME
WELL_FILE_NAME1
WELL_FILE_NAME2
OUTPUT_WELL_FILE_NAME
FORMAT_WELL_FILE1   (usually is FREE)
FORMAT_WELL_FILE2   (usually is FREE)
FORMAT_NEW_WELL_FIL (usually is FREE)
NPER
# -------------- for each stress period -------------- #
NZONE
Zone1 Multiplier1a Multiplier1b
Zone2 Multiplier2a Multiplier2b
'''
ZoneN MultiplierNa MultiplierNb
```
According to the `config` file, the new pumping rate will be `WELL1 * Multiplier1a + WELL2 * Multiplier1b` in `Zone1`.

Note that the zone file needs to follow the format specified in `zone budget`.



