# River

River is an automation library for managing workflow. The intent is to have a
consistent way for tickets to progress through development, QA, and release.
Ideally this can help developers to converge on similar processes across different
groups, keep decided processes consistent, and generate statistics useful to improving
development practices.

## How to Install

1. Add a `.river.env.json` file to the root of your project with the necessary personal
   information (details below). This _should not_ be stored in git.
2. Add a `.river.json` file as shown below. This is project-specific configuration
   and _should_ be added to to git control.
3. Clone this repository locally.
4. Add `river/bin` to your `PATH` to use globally.

Example .river.env.json file. App passwords and api tokens are generated in the settings of your
Jira and Bitbucket accounts. Bitbucket will require _Account:read_ and _PullRequests:read,write_ access.

```
{
    "bitbucketUsername": "MyName",
    "bitbucketPassword": "{BITBUCKET_APP_PASSWORD}",
    "jiraEmail": "{JIRA_EMAIL}",
    "jiraToken": "{JIRA_API_TOKEN}"
}

```

Example .river.json file. Do not populate `defaultReviewers` manually, use `river init` (details below):

```
{
    "repoName": "{REPO_NAME}",
    "defaultReviewers": [],
    "workingBranch": "master",
    "projectKey": "{JIRA_KEY}"
}

```

## Usage

### river -h

Use the help file for details about use.

### river init

Assuming you've set up your credentials, `river init` adds you as a default reviewer for the repo. New
privacy rules on Atlassian means this must be done by individual users on an opt-in basis, but this automates
the process.

### river begin

`river begin` takes a jira ticket key and starts it, setting up a normalized
branch name of your choosing.

Procedure:

1. Branches off of a freshly pulled working branch as specified in `.river` config.
2. Syncs new branch with `remote origin`
3. Sets Jira issue to "In Progress"
4. Assigns Jira issue to you

Basic example: `river begin -k DSP-3723`
Quick-fix examples:
`river begin --quick-fix "quick task, this makes a ticket!"

`river begin --qb "quick bux fix, makes a ticket!"`

### river pr

`river pr` takes your current branch, starts a PR, and sets your Jira ticket to Code review,
pinging all default reviewers.
