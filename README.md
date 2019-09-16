# River

River is an automation library for managing workflow. The intent is to have a
consistent way for tickets to progress through development, QA, and release.
Ideally this can help developers to converge on similar processes across different
groups, keep decided processes consistent, and generate statistics useful to improving
development practices.

## Development Roadmap

For pragmatic reasons, development requires real-life applications in order to meaningfully make progress.
Configuration options are expected to increase over time to support different options, and possibly
allow different tools to be managed as custom plugins. [Trunk based development][tbd] will be the initial workflow supported,
with Git Flow being the most likely next workflow to be added. However, existing use cases will tend to be preferred over
abstract ones.

Currently managed tools:

1. Version control and branch management - Git
2. Project management - Jira
3. Repository management - Github, Bitbucket
4. Bug tracking - Internal tooling

## How to Install

To have `river` available globally -

```
npm install -g river-cli
```

Or to try out River without the installation, use [npx][npx].

```
npx river-cli
```

Check the [Releases][releases] page if you prefer to download and install the binaries directly.

## Configuration

At the root of your project, run:

```
river init
```

That's it. The program will check what information is available, what's missing, ask you for
any required data, and write a properly formatted config file which you can modify as desired. This tool is also meant to serve
as a seamless upgrade tool in case of breaking version changes. There is also an example
configuration which can be used to see what the final file should look like. You can freely modify the configurations after
they are generated.

`.river.env.json` is where your private or personal settings for the project are stored. This _should not_ be stored
in your git history. `river init` will attempt to add the file to your project `.gitignore` file but please ensure it is since
it will include your credentials for things like Jira and Bitbucket.

`.river.json` contains project-level settings and should be committed.

`river gui` is an alternative for those who would prefer a more straightforward form to input this information. This is likely
the future of initializing configuration, but please note that it is currently less fully featured than `river init` or direct
modification of the JSON files.

## On Passwords

App passwords and api tokens are generated in the settings of your
Jira/Github/Bitbucket/etc accounts. Bitbucket will require _Account:read_ and _PullRequests:read,write_ access.

## Usage

### river -h

Use the help file for details about use. This should be your first stop for details
about things like command line flags.

### river init

`river init` will ask you for configuration information and query the API's of
associated tools to gather more complicated details like user ids and Jira transition names
behind the scenes.

### river gui

`river gui` will bring up a graphical interface. Its functionality currently overlaps with `river init`.

### river begin

`river begin` takes a jira ticket key, sets a normalized branch name,
assigns the task to you, and moves the ticket as specified.

Basic example: `river begin -k DSP-3723`
Quick-fix examples:
`river begin --quick-fix "quick task, this makes a ticket!"

`river begin --qb "quick bux fix, makes a ticket!"`

### river pr

`river pr` takes your current branch, starts a PR with all configured reviewers,
and moves your Jira ticket as specified.

### river merge

`river merge` takes your current branch, merges the associated PR, and moves your Jira ticket as requested.

## Regarding Problems and Things

1. It's impossible to support everyone's flows or tools, and this isn't meant to be
   infinitely configurable. But it is meant to be _generally_ applicable, work for most teams,
   and encourage best practices. If you feel a relevant option is missing, please make an
   issue on Github. While I may not be able to promise it will be implemented, I do promise
   I'll address it :)
2. A debug flag (`-d`) is available for all actions. While it is not meant for external users, it would
   be useful information to add if you have an issue to raise.

[tbd]: https://trunkbaseddevelopment.com/
[releases]: https://github.com/DerekMaffett/river/releases
[npx]: https://github.com/zkat/npx
