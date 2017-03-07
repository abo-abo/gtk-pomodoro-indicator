#!/usr/bin/python
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
    def __init__ (self, icon, minutes):
        self.app = "pomodoro-indicator"
        self.start_time = time.time ()
        self.max_time = minutes*60
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
        while delta < self.max_time and not self.update.stopped ():
            time.sleep (1)
            delta = int (time.time () - self.start_time)
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

def project_expand (f):
    return os.path.join (os.path.dirname (os.path.abspath (__file__)), f)

#* Script
if len (sys.argv) != 3:
    print ("Usage: INDICATE [pb] minutes")
    sys.exit (1)

signal.signal (signal.SIGINT, signal.SIG_DFL)
icon_type = sys.argv[1]
minutes = int (sys.argv[2])

if icon_type == "p":
    icon_file = project_expand ("icons/stopwatch.svg")
elif icon_type == "b":
    icon_file = project_expand ("icons/coffee.svg")
else:
    print ("Unknown switch", icon_type)

PomodoroIndicator (icon_file, minutes)
GObject.threads_init ()
gtk.main ()
