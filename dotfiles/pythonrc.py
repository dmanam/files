import atexit
import os
import readline
from xdg import BaseDirectory

histfile = os.path.join(os.path.join(os.path.expanduser(BaseDirectory.xdg_cache_home), "python"), "history")

try:
    readline.read_history_file(histfile)
except FileNotFoundError:
    open(histfile, 'wb').close()

atexit.register(readline.write_history_file, histfile)
