#!/bin/python

# Gcalctool Automated Tests
#
# $Header$
#
# Copyright (c) 1987-2007 Sun Microsystems, Inc.
# All Rights Reserved.
#

"""Gcalctool Automated Tests.  This standalone script talks
directly with the AT-SPI Registry via its IDL interfaces.

It's based on the Orca tool play_keystrokes.py.

It will read gcalctool test calculations (read from standard input) 
and generate keyboard events for the gcalctool application.

To perform the gcalctool automated tests, follow these steps:

1) Run the runtests.py script in a terminal window.
   Results are written to standard output. For example:

       runtests.py > output.txt

2) Start the play_keystrokes.py script in a terminal window.
   The input file should be provided on standard input. For example:

       play_keystrokes.py < input.txt

3) Run gcalctool.

4) Give focus to gcalctool. 

That's it! The tests will now be automatically run and automatically
terminate when the last line from the input file is read.
"""

import os
import signal
import sys
import time
import bonobo
import ORBit
import gtk

ORBit.load_typelib("Accessibility")
ORBit.CORBA.ORB_init()

import Accessibility
import Accessibility__POA

listeners = []
keycodeCache = {}

registry = bonobo.get_object("OAFIID:Accessibility_Registry:1.0",
                             "Accessibility/Registry")

applicationName = "gcalctool"
debug = False

eventTypes = [
    "focus:"
]


########################################################################
#                                                                      #
# Event listener class for global events                               #
#                                                                      #
########################################################################

class EventListener(Accessibility__POA.EventListener):
    """Registers a callback directly with the AT-SPI Registry for the
    given event type.  Most users of this module will not use this
    class directly, but will instead use the addEventListener method.
    """

    def __init__(self, registry, callback, eventType):
        self.registry  = registry
        self.callback  = callback
        self.eventType = eventType
        self.register()

    def ref(self): pass

    def unref(self): pass

    def queryInterface(self, repo_id):
        thiz = None
        if repo_id == "IDL:Accessibility/EventListener:1.0":
            thiz = self._this()
        return thiz

    def register(self):
        self._default_POA().the_POAManager.activate()
        self.registry.registerGlobalEventListener(self._this(),
                                                  self.eventType)
        self.__registered = True
        return self.__registered

    def deregister(self):
        if not self.__registered:
            return
        self.registry.deregisterGlobalEventListener(self._this(),
                                                    self.eventType)
        self.__registered = False

    def notifyEvent(self, event):
        self.callback(event)

    def __del__(self):
        self.deregister()


########################################################################
#                                                                      #
# Testing functions.                                                   #
#                                                                      #
########################################################################

def start():
    """Starts event notification with the AT-SPI Registry.  This method
    only returns after 'stop' has been called.
    """

    bonobo.main()


def stop():
    """Stop event notification with the AT-SPI Registry.
    """

    bonobo.main_quit()


def registerEventListener(callback, eventType):
    """Registers the given eventType and callback with the Registry.

    Arguments:
    - callback: function to call with an AT-SPI event instance
    - eventType: string representing the type of event
    """

    listener = EventListener(registry, callback, eventType)
    listeners.append(listener)


def shutdownAndExit(signum=None, frame=None):
    stop()


def isApplication(event, appName):
    """Check to see if this event is for the desired application, by
    getting the component at the top of the object hierarchy (which
    should have a role of "application", and comparing its name against
    the one given.

    Arguments:
    - event: the event to process
    - appName: the application name to test against
    """

    parent = event.source
    while parent:
        if parent.getRoleName() == "application":
            break
        parent = parent.parent
    if parent and parent.name == appName:
        return True

    return False


def getKeycode(keysym):
    """Converts an XKeysym string (e.g., 'KP_Enter') to a keycode that
    should match the event.hw_code for key events.

    Arguments:
    - keysym: a string that is a valid representation of an XKeysym.

    Returns an integer representing a key code that should match the
    event.hw_code for key events.
    """

    if debug:
        sys.stderr.write("getKeycode: keysym: %s\n" % keysym)

    if not keycodeCache.has_key(keysym):
        keymap = gtk.gdk.keymap_get_default()
        entries = keymap.get_entries_for_keyval(
            gtk.gdk.keyval_from_name(keysym))
        if entries:
            keycodeCache[keysym] = entries[0][0]
        else:
            keycodeCache[keysym] = 0
    return keycodeCache[keysym]


def generateEvents(d, hw_code):
    """Converts an XKeysym event string to its hardware code and generates
    two keyboard events (pressed and released) for it.

    Arguments:
    - d: a handle to the Registry device event controller
    - hw_code: the hardware key code.
    """

    if debug:
        sys.stderr.write("generateEvents: hw_code: %d\n" % hw_code)
    d.generateKeyboardEvent(hw_code, "", 0)
    d.generateKeyboardEvent(hw_code, "", 1)


def sendKey(d, token):
    """Converts an XKeysym event string to its hardware code and generates
    two keyboard events (pressed and released) for it. Look for tokens
    starting with "Control-" and "Alt-" and send two sets of events.

    Arguments:
    - d: a handle to the Registry device event controller
    - token: an XKeysym string containing the event type.
    """

    if debug:
        sys.stderr.write("sendKey: token: %s\n" % token)

    if token.startswith("Control-"):
        generateEvents(d, getKeycode("Control_L"))
        generateEvents(d, getKeycode(token[len(token)-1]))

    elif token.startswith("Alt-"):
        generateEvents(d, getKeycode("Alt_L"))
        generateEvents(d, getKeycode(token[len(token)-1]))

    else:
        generateEvents(d, getKeycode(token))


def readAndSendInput():
    """Keep reading a line of text from standard input (which contains 
    a single gcalctool test), until all lines have been read. Comment
    (lines starting with "#") and blank lines are thrown away. For each 
    remaining line, it splits it into tokens, each of which is an XKeysym 
    event type. Two keyboard events are generated from each of these 
    ("pressed" and "released") until a Return event token has been found. 
    After that has been sent, a pair of Delete events are generated to 
    clear the display before the next line is read from standard input.

    Note that this routine throws a EOFError exception when there is no
    more input to read.
    """

    d = registry.getDeviceEventController()

    while True:
        line = raw_input()
        if (len(line) == 0) or (line[0] == '#'):
            continue

        tokens = line.split()
        for i in range(0, len(tokens)):
            sendKey(d, tokens[i])
            if debug:
                sys.stderr.write("readAndSendInput: sleeping...\n")

            time.sleep(0.5)
            if debug:
                sys.stderr.write("readAndSendInput: waking...\n")

            if tokens[i] == "Return":
                break


def notifyEvent(event):
    if event.type == "focus:":
        if isApplication(event, applicationName):
            sys.stderr.write("Starting keyboard event generation.\n")

            try:
                readAndSendInput()
            except EOFError:
                pass

            sys.stderr.write("Completing keyboard event generation.\n")
            shutdownAndExit()


def test():
    for eventType in eventTypes:
        registerEventListener(notifyEvent, eventType)
    start()


if __name__ == "__main__":
    import signal
    signal.signal(signal.SIGINT, shutdownAndExit)
    signal.signal(signal.SIGQUIT, shutdownAndExit)
    test()
