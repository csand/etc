#!/usr/bin/env python
"""in-progress

Queries the JIRA API for In Progress tickets and sets $JIRA_ISSUE to the
ticket's issue number. Prompts for a user's choice if multiple tickets are In
Progress.
"""

import os
from os import environ as ENV
from jira.client import JIRA


def main():
    username = ENV['JIRA_USERNAME']
    password = ENV['JIRA_PASSWORD']
    host     = ENV['JIRA_HOST']
    jira = JIRA({'server': host}, basic_auth=(username, password))
    query = 'assignee=currentUser() and status="In Progress"'
    in_progress = jira.search_issues(query)
    if len(in_progress) > 1:
        while True:
            for i, issue in enumerate(in_progress):
                print '{0}: {1}, {2}'.format(i + 1, issue.key, issue.fields.summary)

            n = raw_input('Please choose one of the above, [1-{0}]: '.format(
                                                            len(in_progress)))
            try:
                n = int(n)
                active = in_progress[n - 1]
            except:
                print 'Not a valid choice'
            else:
                break
    else:
        active = in_progress[0]

    print 'Activating {0} for commit messages.'.format(active.key)
    with open(os.path.expanduser('~/.jiraissue'), 'w') as f:
        f.write("export JIRA_ISSUE='{0}'".format(active.key))
    os.system('source ~/.jiraissue')

if __name__ == '__main__':
    main()
