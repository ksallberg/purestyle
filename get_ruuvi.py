#!/usr/bin/python3
import signal
from ruuvitag_sensor.ruuvitag import RuuviTag

def timeout_handler(signum, frame):
    raise TimeoutError("Sensor read timed out")

signal.signal(signal.SIGALRM, timeout_handler)
signal.alarm(20)  # 20 seconds timeout

try:
    sensor = RuuviTag("F9:09:24:57:1E:87")
    state = sensor.update()
    signal.alarm(0)  # cancel alarm if successful
    print(state)
except TimeoutError as e:
    print(e)
