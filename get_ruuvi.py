#!/usr/bin/python3
from ruuvitag_sensor.ruuvitag import RuuviTag

sensor = RuuviTag("F9:09:24:57:1E:87")

# update state from the device
state = sensor.update()

# get latest state (does not get it from the device)
state = sensor.state

print(state)
