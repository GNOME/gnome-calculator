# -*- coding: UTF-8 -*-
from behave import step, then

from dogtail.tree import root
from dogtail.rawinput import typeText
from common_steps import limit_execution_time_to, wait_until
from time import sleep


@step(u'Help section "{name}" is displayed')
def help_is_displayed(context, name):
    context.yelp = root.application('yelp')
    frame = context.yelp.child(roleName='frame')
    wait_until(lambda x: x.showing, frame)
    sleep(1)
    context.assertion.assertEquals(name, frame.name)


@step(u'Switch to {mode:w} mode')
def switch_to_basic_mode(context, mode):
    context.app.child(roleName='toggle button', name='Menu').click()
    context.app.child(roleName='radio button', name='%s Mode' % mode).click()


@limit_execution_time_to(seconds=30)
def wait_until_spinner_showing(context):
    # Wait until spinner disappears
    sleep(0.1)
    spinner = context.app.child('Spinner')
    while spinner.showing:
        sleep(0.1)


@step(u'Calculate "{expression}"')
def calculate(context, expression):
    typeText(expression)
    context.app.child('result').click()
    wait_until_spinner_showing(context)


@step(u'result is "{result}"')
def verify_result(context, result):
    # Replace unicode negative sign to simple '-'
    actual = context.app.child(roleName='editbar').text.replace('\xe2\x88\x92', '-')
    assert result == actual, "Incorrect result, expected '%s' but was '%s'" % (result, actual)


@then(u'"{message}" error is displayed')
def error_message_displayed(context, message):
    actual = context.app.child(roleName='editbar').parent.textentry('').text
    assert message == actual, "Incorrect error, expected '%s' but was '%s'" % (message, actual)
