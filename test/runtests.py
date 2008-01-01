#!/bin/python

# Gcalctool Automated Tests
#
# $Header$
#
# Copyright (c) 1987-2008 Sun Microsystems, Inc.
# All Rights Reserved.
#

"""Gcalctool Automated Tests.  This standalone script talks
directly with the AT-SPI Registry via its IDL interfaces.

It's based on the bug scripts created by Will Walker for accessibility 
problems found with various applications when interacting with Orca, 
the screen reader/magnifier.

To perform the gcalctool automated tests, follow these steps:

1) Run the runtests.py script in a terminal window.
   Results are written to standard output. For example:

       runtests.py > output.txt

2) Start the play_keystrokes.py script in a terminal window.
   The input file should be provided on standard input. For example:

       play_keystrokes.py < input.txt

3) Run gcalctool. 

4) Give focus to gcalctool.

That's it! The tests will now be automatically run. Currently you will
need to type Control-C to terminal the runtest.py script when the
play_keystrokes.py script automatically terminates.
"""

import time
import bonobo
import ORBit
import threading
import gtk
import sys

ORBit.load_typelib("Accessibility")
ORBit.CORBA.ORB_init()

import Accessibility
import Accessibility__POA

listeners = []
keystrokeListeners = []

registry = bonobo.get_object("OAFIID:Accessibility_Registry:1.0",
                             "Accessibility/Registry")

applicationName = "gcalctool"
debug = True
display = None
lastKeyEvent = None


########################################################################
#                                                                      #
# Event listener classes for global and keystroke events               #
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


    def ref(self): 
        pass


    def unref(self): 
        pass


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


class KeystrokeListener(Accessibility__POA.DeviceEventListener):
    """Registers a callback directly with the AT-SPI Registry for the
    given keystroke.  Most users of this module will not use this
    class directly, but will instead use the registerKeystrokeListeners
    method."""

    def keyEventToString(event):
        return ("KEYEVENT: type=%d\n" % event.type) \
               + ("          hw_code=%d\n" % event.hw_code) \
               + ("          modifiers=%d\n" % event.modifiers) \
               + ("          event_string=(%s)\n" % event.event_string) \
               + ("          is_text=%s\n" % event.is_text) \
               + ("          time=%f" % time.time())


    keyEventToString = staticmethod(keyEventToString)


    def __init__(self, registry, callback,
                 keyset, mask, type, synchronous, preemptive, isGlobal):
        self._default_POA().the_POAManager.activate()

        self.registry         = registry
        self.callback         = callback
        self.keyset           = keyset
        self.mask             = mask
        self.type             = type
        self.mode             = Accessibility.EventListenerMode()
        self.mode.synchronous = synchronous
        self.mode.preemptive  = preemptive
        self.mode._global     = isGlobal
        self.register()


    def ref(self): 
        pass


    def unref(self):
        pass


    def queryInterface(self, repo_id):
        thiz = None
        if repo_id == "IDL:Accessibility/EventListener:1.0":
            thiz = self._this()

        return thiz


    def register(self):
        d = self.registry.getDeviceEventController()
        if d.registerKeystrokeListener(self._this(), self.keyset,
                                       self.mask, self.type, self.mode):
            self.__registered = True
        else:
            self.__registered = False

        return self.__registered


    def deregister(self):
        if not self.__registered:
            return
        d = self.registry.getDeviceEventController()
        d.deregisterKeystrokeListener(self._this(), self.keyset,
                                      self.mask, self.type)
        self.__registered = False


    def notifyEvent(self, event):
        """Called by the at-spi registry when a key is pressed or released.

        Arguments:
        - event: an at-spi DeviceEvent

        Returns True if the event has been consumed.
        """

        return self.callback(event)


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
    """Unregisters any event or keystroke listeners registered with
    the AT-SPI Registry and then stops event notification with the
    AT-SPI Registry.
    """

    for listener in (listeners + keystrokeListeners):
        listener.deregister()
    bonobo.main_quit()


def registerEventListener(callback, eventType):
    global listeners

    listener = EventListener(registry, callback, eventType)
    listeners.append(listener)


def registerKeystrokeListeners(callback):
    """Registers a single callback for all possible keystrokes.
    """

    global keystrokeListeners

    for i in range(0, (1 << (Accessibility.MODIFIER_NUMLOCK + 1))):
        keystrokeListeners.append(
            KeystrokeListener(registry,
                              callback, # callback
                              [],       # keyset
                              i,        # modifier mask
                              [Accessibility.KEY_PRESSED_EVENT,
                               Accessibility.KEY_RELEASED_EVENT],
                              True,     # synchronous
                              True,     # preemptive
                              False))   # global


########################################################################
#                                                                      #
# Helper utilities.                                                    #
#                                                                      #
########################################################################

def getStateString(acc):
    """Returns a space-delimited string composed of the given object's
    Accessible state attribute.  This is for debug purposes.
    """

    s = acc.getState()
    s = s._narrow(Accessibility.StateSet)
    stateSet = s.getStates()
    
    stateString = " "
    if stateSet.count(Accessibility.STATE_INVALID):
        stateString += "INVALID "
    if stateSet.count(Accessibility.STATE_ACTIVE):
        stateString += "ACTIVE "
    if stateSet.count(Accessibility.STATE_ARMED):
        stateString += "ARMED "
    if stateSet.count(Accessibility.STATE_BUSY):
        stateString += "BUSY "
    if stateSet.count(Accessibility.STATE_CHECKED):
        stateString += "CHECKED "
    if stateSet.count(Accessibility.STATE_COLLAPSED):
        stateString += "COLLAPSED "
    if stateSet.count(Accessibility.STATE_DEFUNCT):
        stateString += "DEFUNCT "
    if stateSet.count(Accessibility.STATE_EDITABLE):
        stateString += "EDITABLE "
    if stateSet.count(Accessibility.STATE_ENABLED):
        stateString += "ENABLED "
    if stateSet.count(Accessibility.STATE_EXPANDABLE):
        stateString += "EXPANDABLE "
    if stateSet.count(Accessibility.STATE_EXPANDED):
        stateString += "EXPANDED "
    if stateSet.count(Accessibility.STATE_FOCUSABLE):
        stateString += "FOCUSABLE "
    if stateSet.count(Accessibility.STATE_FOCUSED):
        stateString += "FOCUSED "
    if stateSet.count(Accessibility.STATE_HAS_TOOLTIP):
        stateString += "HAS_TOOLTIP "
    if stateSet.count(Accessibility.STATE_HORIZONTAL):
        stateString += "HORIZONTAL "
    if stateSet.count(Accessibility.STATE_ICONIFIED):
        stateString += "ICONIFIED "
    if stateSet.count(Accessibility.STATE_MODAL):
        stateString += "MODAL "
    if stateSet.count(Accessibility.STATE_MULTI_LINE):
        stateString += "MULTI_LINE "
    if stateSet.count(Accessibility.STATE_MULTISELECTABLE):
        stateString += "MULTISELECTABLE "
    if stateSet.count(Accessibility.STATE_OPAQUE):
        stateString += "OPAQUE "
    if stateSet.count(Accessibility.STATE_PRESSED):
        stateString += "PRESSED "
    if stateSet.count(Accessibility.STATE_RESIZABLE):
        stateString += "RESIZABLE "
    if stateSet.count(Accessibility.STATE_SELECTABLE):
        stateString += "SELECTABLE "
    if stateSet.count(Accessibility.STATE_SELECTED):
        stateString += "SELECTED "
    if stateSet.count(Accessibility.STATE_SENSITIVE):
        stateString += "SENSITIVE "
    if stateSet.count(Accessibility.STATE_SHOWING):
        stateString += "SHOWING "
    if stateSet.count(Accessibility.STATE_SINGLE_LINE):
        stateString += "SINGLE_LINE "
    if stateSet.count(Accessibility.STATE_STALE):
        stateString += "STALE "
    if stateSet.count(Accessibility.STATE_TRANSIENT):
        stateString += "TRANSIENT "
    if stateSet.count(Accessibility.STATE_VERTICAL):
        stateString += "VERTICAL "
    if stateSet.count(Accessibility.STATE_VISIBLE):
        stateString += "VISIBLE "
    if stateSet.count(Accessibility.STATE_MANAGES_DESCENDANTS):
        stateString += "MANAGES_DESCENDANTS "
    if stateSet.count(Accessibility.STATE_INDETERMINATE):
        stateString += "INDETERMINATE "

    return stateString.strip()


def getNameString(acc):
    """Return the name string for the given accessible object.

    Arguments:
    - acc: the accessible object

    Returns the name of this accessible object (or "None" if not set).
    """

    if acc.name:
        return "'%s'" % acc.name
    else:
        return "None"


def getAccessibleString(acc):
    return "name=%s role='%s' state='%s'" \
           % (getNameString(acc), acc.getRoleName(), getStateString(acc))


# List of event types that we are interested in.

eventTypes = [
##     "focus:",
##     "mouse:rel",
##     "mouse:button",
##     "mouse:abs",
##     "keyboard:modifiers",
##     "object:property-change",
##     "object:property-change:accessible-name",
##     "object:property-change:accessible-description",
##     "object:property-change:accessible-parent",
##     "object:state-changed",
##     "object:state-changed:focused",
##     "object:selection-changed",
##     "object:children-changed"
##     "object:active-descendant-changed"
##     "object:visible-data-changed"
##     "object:text-selection-changed",
##     "object:text-caret-moved",
##     "object:text-changed",
    "object:text-changed:insert",
##     "object:column-inserted",
##     "object:row-inserted",
##     "object:column-reordered",
##     "object:row-reordered",
##     "object:column-deleted",
##     "object:row-deleted",
##     "object:model-changed",
##     "object:link-selected",
##     "object:bounds-changed",
##     "window:minimize",
##     "window:maximize",
##     "window:restore",
    "window:activate",
##     "window:create",
##     "window:deactivate",
##     "window:close",
##     "window:lower",
##     "window:raise",
##     "window:resize",
##     "window:shade",
##     "window:unshade",
##     "object:property-change:accessible-table-summary",
##     "object:property-change:accessible-table-row-header",
##     "object:property-change:accessible-table-column-header",
##     "object:property-change:accessible-table-summary",
##     "object:property-change:accessible-table-row-description",
##     "object:property-change:accessible-table-column-description",
##     "object:test",
##     "window:restyle",
##     "window:desktop-create",
##     "window:desktop-destroy"
]


def getObjects(root):
    """Returns a list of all objects under the given root.  Objects
    are returned in no particular order - this function does a simple
    tree traversal, ignoring the children of objects which report the
    MANAGES_DESCENDANTS state is active.

    NOTE: this will throw an InvalidObjectError exception if the
    AT-SPI Accessibility_Accessible can no longer be reached via
    CORBA.

    Arguments:
    - root: the Accessible object to traverse

    Returns: a list of all objects under the specified object
    """

    # The list of object we'll return
    #
    objlist = []

    # Start at the first child of the given object
    #
    if root.childCount <= 0:
        return objlist

    for i in range(0, root.childCount):
        child = root.getChildAtIndex(i)
        if child:
            objlist.append(child)
            s = child.getState()
            s = s._narrow(Accessibility.StateSet)
            state = s.getStates()
            if (state.count(Accessibility.STATE_MANAGES_DESCENDANTS) == 0) \
                   and (child.childCount > 0):
                objlist.extend(getObjects(child))

    return objlist


def findByRole(root, role):
    """Returns a list of all objects of a specific role beneath the
    given root.

    NOTE: this will throw an InvalidObjectError exception if the
    AT-SPI Accessibility_Accessible can no longer be reached via
    CORBA.

    Arguments:
    - root the Accessible object to traverse
    - role the string describing the Accessible role of the object

    Returns a list of descendants of the root that are of the given role.
    """

    objlist = []
    allobjs = getObjects(root)
    for o in allobjs:
        if o.getRoleName() == role:
            objlist.append(o)
    return objlist


def getDisplayText(obj):
    """Returns the calculator display value.

    Arguments:
    - obj a handle to the calculator display component

    Returns a string containing the current value of the calculator display.
    """

    if not obj:
        return
    text = obj.queryInterface("IDL:Accessibility/Text:1.0")
    if text:
        text = text._narrow(Accessibility.Text)
        return text.getText(0, -1)


def notifyEvent(event):
    global applicationName, debug, display, lastKeyEvent, registry

    # We are interested in two types of events.
    # 1) window:activate
    # 2) object:text-changed:insert

    # 1) window:activate
    #
    # If we get a "window:activate" event for the gcalctool application
    # then (we we haven't already done it), get a handle to the "edit bar" 
    # gcalctool component. This will contain the calculator display.
    # If we can't find it, terminate.

    if event.type == "window:activate":
        if isApplication(event, applicationName):
            if (display is None) and (event.source.getRoleName() == "frame"):
                d = findByRole(event.source, "edit bar")

                if len(d) == 0:
                    sys.stderr.write("Unable to get calculator display.\n")
                    shutdownAndExit()

                if debug:
                    sys.stderr.write("Caching display component handle.\n")
                display = d[0]


    # 2) object:text-changed:insert
    #
    # If we get a "object:text-changed:insert" for the gcalctool application
    # and the source of the event is the display component, then, if the
    # last event was "Return" (and this isn't the first time), get the 
    # contents of the display and print it to standard output.

    if event.type == "object:text-changed:insert":
        if isApplication(event, applicationName):
            if event.source == display:
                if lastKeyEvent == None:
                    return

                if debug:
                    sys.stderr.write("MATCH: Last key event %s\n" % \
                                     lastKeyEvent.event_string)

                if lastKeyEvent.event_string == "Return":
                    if debug:
                        sys.stderr.write("Printing result: %s\n" % \
                                         getDisplayText(display))
                    print getDisplayText(display)


def notifyKeystroke(event):
    """Process keyboard events.

    Arguments:
    - event: the keyboard event to process
    """

    global debug, lastKeyEvent

    # print KeystrokeListener.keyEventToString(event)

    # If this is a "pressed" keyboard event (and not an "F12" event), 
    # print its value to standard output. 
    #
    # This is hopefully make the output the same as the input, to allow 
    # the user to easy determine incorrect results by comparing the two 
    # files.

    if event.type == 0:
        if debug:
            sys.stderr.write("notifyKeystroke: %s\n" % event.event_string)
        if (event.event_string != "F12") and \
           (event.event_string != "SunF37"):
            print event.event_string,
        lastKeyEvent = event

    # If the user has deliberately hit the F12 key, then terminate the
    # application.

    if (event.event_string == "F12") or (event.event_string == "SunF37"):
        shutdownAndExit()

    return False


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


def test():
    for eventType in eventTypes:
        registerEventListener(notifyEvent, eventType)
    registerKeystrokeListeners(notifyKeystroke)
    start()


if __name__ == "__main__":
    import signal
    signal.signal(signal.SIGINT, shutdownAndExit)
    signal.signal(signal.SIGQUIT, shutdownAndExit)
    test()
