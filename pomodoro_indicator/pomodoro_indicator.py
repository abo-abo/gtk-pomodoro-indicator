#* Info
# Copyright (C) 2017 Oleh Krehel
# Author: Oleh Krehel <ohwoeowho@gmail.com>
# URL: https://github.com/abo-abo/gtk-pomodoro-indicator
# Version: 0.1.0
# Keywords: pomodoro

# This file is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# For a full copy of the GNU General Public License
# see <http://www.gnu.org/licenses/>.

#* Imports
import os
import signal
import gi
gi.require_version ("Gtk", "3.0")
gi.require_version ("AppIndicator3", "0.1")
from gi.repository import Gtk as gtk
from gi.repository import AppIndicator3 as appindicator
from gi.repository import GObject
import time
import threading
import sys

#* Class
class StoppableThread (threading.Thread):
    def __init__ (self, target):
        super (StoppableThread, self).__init__ (target = target)
        self._stop = threading.Event ()

    def stop (self):
        self._stop.set ()

    def stopped (self):
        return self._stop.isSet ()

class PomodoroIndicator ():
    def __init__ (self, icon, minutes, count_up = False):
        self.app = "pomodoro-indicator"
        self.start_time = time.time ()
        self.max_time = minutes*60
        self.count_up = count_up
        self.indicator = appindicator.Indicator.new (
            self.app,
            icon,
            category = appindicator.IndicatorCategory.APPLICATION_STATUS)
        self.indicator.set_status (appindicator.IndicatorStatus.ACTIVE)
        self.indicator.set_menu (self.build_menu ())
        self.indicator.set_label ("%02d" % (self.max_time/60), self.app)
        self.update = StoppableThread (target = self.show_seconds)
        # daemonize the thread to make the indicator stopable
        self.update.setDaemon (True)
        self.update.start ()

    def build_menu (self):
        menu = gtk.Menu ()
        item_quit = gtk.MenuItem ("Quit")
        item_quit.connect ("activate", self.stop)
        menu.append (item_quit)
        menu.show_all ()
        return menu

    def show_seconds (self):
        delta = 0
        while self.count_up or (delta < self.max_time and not self.update.stopped ()):
            time.sleep (1)
            delta = int (time.time () - self.start_time)
            if self.count_up:
                mention = "%02d" % ((delta + self.max_time) / 60)
            else:
                left = self.max_time - delta
                if left > 60:
                    mention = "%02d" % (left / 60 + 1)
                else:
                    mention = "00:%02d" % left
            # apply the interface update using  GObject.idle_add ()
            GObject.idle_add (
                self.indicator.set_label,
                mention, self.app,
                priority = GObject.PRIORITY_DEFAULT)
        gtk.main_quit ()

    def stop (self, source):
        self.update.stop ()
        gtk.main_quit ()

def get_icon(f):
    icons_dir = "/usr/local/pomodoro-indicator"
    if not os.path.exists(icons_dir):
        project_dir = os.path.dirname (os.path.abspath (__file__))
        icons_dir = os.path.join(project_dir, "../icons")
    return os.path.realpath(os.path.join(icons_dir, f))

def main(argv = None):
    argv = argv or sys.argv
    if len (argv) != 3:
        print ("Usage: INDICATE [pbu] minutes")
        sys.exit (1)

    signal.signal (signal.SIGINT, signal.SIG_DFL)
    icon_type = argv[1]
    minutes = int (argv[2])
    if icon_type == "p" or icon_type == "u":
        icon_file = get_icon("stopwatch.svg")
    elif icon_type == "b":
        icon_file = get_icon("coffee.svg")
    else:
        print ("Unknown switch", icon_type)

    PomodoroIndicator (icon_file, minutes, icon_type == "u")
    GObject.threads_init ()
    gtk.main ()

#* Script
if __name__ == '__main__':
    main(sys.argv)
