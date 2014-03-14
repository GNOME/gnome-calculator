# -*- coding: UTF-8 -*-

from time import sleep
from dogtail.utils import isA11yEnabled, enableA11y
if not isA11yEnabled():
    enableA11y(True)

from common_steps import App
from dogtail.config import config


def before_all(context):
    """Setup gnome-calculator stuff
    Being executed before all features
    """

    try:
        # Skip dogtail actions to print to stdout
        config.logDebugToStdOut = False
        config.typingDelay = 0.2

        context.app_class = App('gnome-calculator')

    except Exception as e:
        print("Error in before_all: %s" % e.message)


def after_scenario(context, scenario):
    """Teardown for each scenario
    Kill gnome-calculator (in order to make this reliable we send sigkill)
    """
    try:
        # Stop gnome-calculator
        context.app_class.kill()

        # Make some pause after scenario
        sleep(1)
    except Exception as e:
        # Stupid behave simply crashes in case exception has occurred
        print("Error in after_scenario: %s" % e.message)
