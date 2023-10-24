# Welcome to this repository's contributing guide

Thank you for investing your time in contributing to this project!
All contributions are welcome as long as they serve to make this package safer, easier to use, more feature-rich and stable. Any contribution you make will be reflected on  [github.com/m-jahn/WeightedTreemaps](https://github.com/m-jahn/WeightedTreemaps).

Read the [Code of Conduct](./CODE_OF_CONDUCT.md) to keep the developer community approachable and respectable.

In this guide you will get an overview of the contribution workflow from opening an issue, creating a pull request (PR), reviewing, and merging the PR.

## New contributor guide

To get an overview of the project, read the [README](README.md) file. Here are some resources to help you get started with open source contributions:

- [Finding ways to contribute to open source on GitHub](https://docs.github.com/en/get-started/exploring-projects-on-github/finding-ways-to-contribute-to-open-source-on-github)
- [Set up Git](https://docs.github.com/en/get-started/quickstart/set-up-git)
- [GitHub flow](https://docs.github.com/en/get-started/quickstart/github-flow)
- [Collaborating with pull requests](https://docs.github.com/en/github/collaborating-with-pull-requests)


## Getting started

### Issues

#### Create a new issue

If you spot a problem with this package, [search if an issue already exists](https://docs.github.com/en/github/searching-for-information-on-github/searching-on-github/searching-issues-and-pull-requests#search-by-the-title-body-or-comments). If a related issue doesn't exist, you can open a new issue using a relevant [issue form](https://github.com/github/docs/issues/new/choose).

#### Solve an issue

Scan through the [existing issues](https://github.com/m-jahn/WeightedTreemaps/issues) to find one that interests you. You can narrow down the search using `labels` as filters. Issues can be assigned to specific contributors. You may still comment on these issues and make suggestions. If you find an open issue to work on, you are welcome to open a PR with a fix.

### Make Changes

1. Fork the repository.
- Using GitHub Desktop:
  - [Getting started with GitHub Desktop](https://docs.github.com/en/desktop/installing-and-configuring-github-desktop/getting-started-with-github-desktop) will guide you through setting up Desktop.
  - Once Desktop is set up, you can use it to [fork the repo](https://docs.github.com/en/desktop/contributing-and-collaborating-using-github-desktop/cloning-and-forking-repositories-from-github-desktop)!

- Using the command line:
  - [Fork the repo](https://docs.github.com/en/github/getting-started-with-github/fork-a-repo#fork-an-example-repository) so that you can make your changes without affecting the original project until you're ready to merge them.
  - Clone your local fork of the repo using git: `git clone https://github.com/<your_user_name>/WeightedTreemaps`

2. Create a working branch and start with your changes (this section applies to a command line workflow)

 - Using git locally: `git checkout -b newbranch`

### Commit your update

(this section applies to a command line workflow)

1. start making changes, and test them by checking that all examples from the vignette behave as expected.
2. For an R package, run `R CMD CHECK` or `Build` --> `Check package` in [Rstudio](https://posit.co/download/rstudio-desktop/). It should show no errors, ideally no warnings (except generic ones), and only negligible notes.
3. Commit the changes once you are happy with them using `git add <my_changed_file>` and then `git commit -m "fix: that bug, closes #23"`. Don't forget to add a commit message that uses industry standard prefixes such as `fix:`, `feat:`, and `docs:`. Link the issue you are solving by using number tags (`#23`)
4. Push your changed branch to your own fork using `git push origin newbranch`

### Pull Request

1. When you are happy with your changes, notify the other contributors/maintainers in the [main repository's issue](https://github.com/m-jahn/WeightedTreemaps/issues) you like to solve.
2. If your contribution is deemed welcome and ready to be merged, create a pull request, also known as a PR. Do not request the `main/master` branch but the `dev` or other applicable feature branch to be used for merging. The main branch is protected and should only be merged from the [WeightedTreemaps' dev](https://github.com/m-jahn/WeightedTreemaps/tree/dev).
3. Once you submit your PR, a maintainer will review your proposal. They may ask questions or request additional information.
- Changes may be requested before a PR can be merged, either using [suggested changes](https://docs.github.com/en/github/collaborating-with-issues-and-pull-requests/incorporating-feedback-in-your-pull-request) or pull request comments. You can apply suggested changes directly through the UI. You can also make changes in your fork, then commit them to your branch you like to merge. They will be _automatically added_ to your PR.
- As you update your PR and apply changes, mark each conversation as [resolved](https://docs.github.com/en/github/collaborating-with-issues-and-pull-requests/commenting-on-a-pull-request#resolving-conversations).
- If you run into any merge issues, checkout this [git tutorial](https://github.com/skills/resolve-merge-conflicts) to help you resolve merge conflicts and other issues.
4. If local checks pass and your PR adheres to all other standards, it will be merged into the `dev` branch.
5. If automatic checks from Github Actions workflows pass, it will be merged into the protected `main/master` branch.

### Being a Contributor

1. You will be automatically added to the [contributors](https://github.com/m-jahn/WeightedTreemaps/graphs/contributors) page of this repository. Your profile picture will be visible on the front page.
2. Your contributions will show up on your Github activity profile.

Thank you for contributing to this package!
