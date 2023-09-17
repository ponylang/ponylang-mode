# Contributing to Ponylang-mode

Hi there! Thanks for your interest in contributing to Ponylang-mode.

Please note, that by submitting changes to ponylang-mode, you are agreeing that
it can be licensed under our license as seen in `ponylang-mode.el`. Furthermore,
you are testifying that you own the copyright to the submitted changes and
indemnify ponylang-mode from any copyright claim that might result from your not
being the authorized copyright holder.

## Code formatting basics

* 2 Spaces not tabs for indentation
* No lines over 80 columns

## Pull request

While we don't require that your pull request be a single commit, note that we will end up squashing all your commits into a single commit when we merge. While your PR is in review, we may ask for additional changes, please do not squash those commits while the review is underway. We ask that you not squash while a review is underway as it can make it hard to follow what is going on.

When opening your pull request, please make sure that the initial comment on the PR is the commit message we should use when we merge. Making sure your commit message conforms to these guidelines for [writ(ing) a good commit message](http://chris.beams.io/posts/git-commit/).

Make sure to issue 1 pull request per feature. Don't lump unrelated changes together. If you find yourself using the word "and" in your commit comment, you
are probably doing too much for a single PR.

We keep a [CHANGELOG](CHANGELOG.md) of all software changes with behavioural effects in ponyc. If your PR includes such changes (rather than say a documentation update), a Pony team member will do the following before merging it, so that the PR will be automatically added to the CHANGELOG:

* Ensure that the ticket is tagged with one or more appropriate "changelog - *" labels - each label corresponds to a section of the changelog where this change will be automatically mentioned.
* Ensure that the ticket title is appropriate - the title will be used as the summary of the change, so it should be appropriately formatted, including a ticket reference if the PR is a fix to an existing bug ticket.
  * For example, an appropriate title for a PR that fixes a bug reported in issue ticket #98 might look like:
  * *Fixed compiler crash related to tuple recovery (issue #98)*

Once those conditions are met, the PR can be merged, and an automated system will immediately add the entry to the changelog. Keeping the changelog entries out of the file changes in the PR helps to avoid conflicts and other administrative headaches when many PRs are in progress.

Any change that involves a changelog entry will trigger a bot to request that you add release notes to your PR.

Pull requests from accounts that aren't members of the Ponylang organization require approval from a member before running. Approval is required after each update that you make. This could involve a lot of waiting on your part for approvals. If you are opening PRs to verify that changes all pass CI before "opening it for real", we strongly suggest that you open the PR against the `main` branch of your fork. CI will then run in your fork and you don't need to wait for approval from a Ponylang member.
