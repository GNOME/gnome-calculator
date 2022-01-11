/* -*- Mode: Vala; indent-tabs-mode: nil; c-basic-offset: 2; tab-width: 2 -*- */
/*
 * GCalc Unit Tests
 * Copyright (C) Daniel Espinosa Ortiz 2019 <esodan@gmail.com>
 *
 * libgda is free software: you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * libgda is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
using GCalc;
using GCi;

class Tests {
  [GtkTemplate (ui = "/org/gnome/Calculator/gci-test-entry-controller.ui")]
  class Window : Gtk.ApplicationWindow {
    [GtkChild]
    unowned Gtk.Entry entry;
    [GtkChild]
    unowned Gtk.Button button;

    GCi.EntryController controller;

    construct {
      controller = new GCi.EntryController ();
      controller.entry = entry;
      GLib.Timeout.add (10000, ()=>{
        application.quit ();
      });
      button.clicked.connect (()=>{
        message ("Closing...");
        application.quit ();
      });
    }

    public Window (Gtk.Application app) {
      Object(application: app);
    }
  }
  class Application : Gtk.Application {
    public Application () {
		  Object(application_id: "test.gci.entry.controler",
				  flags: ApplicationFlags.FLAGS_NONE);
	  }

	  protected override void activate () {
		  // Create the window of this application and show it
		  Gtk.ApplicationWindow w = new Window (this);
		  w.present ();
	  }
  }
  static int main (string[] args)
  {
    GLib.Intl.setlocale (GLib.LocaleCategory.ALL, "");
    Test.init (ref args);
    Test.add_func ("/gci/entry/controler",
    ()=>{
      var app = new Application ();
      app.run ();
    });
    return Test.run ();
  }
}
