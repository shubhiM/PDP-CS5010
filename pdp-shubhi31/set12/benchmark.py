import subprocess
from datetime import datetime
from datetime import timedelta

start_time = datetime.now()

# returns the elapsed milliseconds since the start of the program
def millis():
   dt = datetime.now() - start_time
   ms = (dt.days * 24 * 60 * 60 + dt.seconds) * 1000 + dt.microseconds / 1000.0
   return ms


subprocess.call("./larceny --r7rs --program sumsq.ps11.scm -- 10000 100000", shell=True)

print "duration"
print millis()


