<!-- omit in toc -->
# Contributing to Jink

<b>Thank you for considering contributing to Jink!</b> Please refer to the [Table of Contents](#table-of-contents) for various ways to contribute and get involved or understand the processes behind doing so. Please familiarize yourself with the process before making your contribution. This will ensure a smoother experience for everyone involved. Thank you!

###### If you have any suggestions for improving this guide, let us know. If you like the project and aren't able to or interested in contributing directly, please consider giving us a ⭐!❤️

## Table of Contents

- [I Have a Question](#i-have-a-question)
- [I Want To Contribute](#i-want-to-contribute)
- [Reporting Bugs](#reporting-bugs)
- [Suggesting Enhancements](#suggesting-enhancements)
- [Your First Contribution](#your-first-contribution)
<!-- - [Improving The Documentation](#improving-the-documentation) -->
- [Style Guide](#style-guide)
- [Commit Messages](#commit-messages)
<!-- - [Join The Project Team](#join-the-project-team) -->

## I Have a Question

<!-- ######  If you want to ask a question, we assume that you have read the available [Documentation](). -->

Before you report an issue or ask a question, it is best to search through existing [issues](https://github.com/jink-lang/jink/issues) for any that might help you, or more specifically, any labelled [questions](https://github.com/jink-lang/jink/labels/question), in case yours has already been answered. If you do find a suitable issue and still need help, you can add your question to that issue to keep things organized. If your issue is related to Rust, you might want to check the [Rust documentation](https://doc.rust-lang.org/book/) first.

If after checking existing issues you still feel the need to ask a question or need further clarification, here is what you can do:

- Open a [new issue](https://github.com/jink-lang/jink/issues/new).
- If you've encountered a problem, provide as much context as you can about what it is, including but not limited to the following information:
  - What you're **trying to do**
  - What you **expected to happen**
  - What **actually** happened, your input/code and **steps to reproduce the issue**
  - Any other information that might be helpful</b>
- If you just have a general question, provide as much context as you can about what you're trying to understand or accomplish.

Someone can then help you as soon as possible.

Optionally, you can also join Jacob's project [Discord server](https://discord.gg/cWzcQz2) and ask your question or open a discussion there, in the Jink category chat.

## I Want To Contribute

### Reporting Bugs

#### Before Submitting a Report

A bug is a demonstrable problem that is caused by the code in the repository. If you are experiencing a problem that you think is a bug, please follow these steps:

- Make sure that you are using the latest version of the project. If you don't know, it might be a good idea to try updating to the latest version to see if the bug still exists.
- Determine if the bug is really a bug and not an error on your end, such as using an incorrect or outdated syntax as the project is in early development. <!-- (Make sure that you have read the [documentation](). --> If you are looking for support, you are looking for [this section](#i-have-a-question) instead.
- To see if anyone else has experienced (or potentially already solved) the same bug you are facing, check if there is not already a report existing for it, or error in the [bug tracker](https://github.com/jink-lang/jink/issues?q=label%3Abug).
- Collect and submit information about the bug:
  - <b>Stack trace (Traceback) if applicable.
  - OS, Platform and Version (Windows, Linux, macOS, x86, ARM)
  - Version of the compiler, Rust version, Cargo version, and any other version information that seems relevant, if applicable.
  - Your input and the verbose (`-v` flag) compiler output.
  - Can you reliably reproduce the issue? And, if you have the time to test, is it reproducible it with older versions?</b>

#### How Do I Submit a Good Bug Report?

###### You must never report security related issues, vulnerabilities or bugs including sensitive information to the issue tracker, or elsewhere in public. Instead sensitive bugs must be sent by email to jacobnoahbusiness@gmail.com. <!-- You may add a PGP key to allow the messages to be sent encrypted if you so wish. Encrypt your message with the key in the [PGP.md](./PGP.md) file. -->

We use GitHub issues to track bugs and errors. If you run into an issue with the project:

- Open a [new issue](https://github.com/jink-lang/jink/issues/new). Since we can't be sure whether it is a bug or not until, we ask you not to talk about it as a bug yet and not to label the issue as such. Instead, use a descriptive title and explain the issue in detail.
- Explain the behavior you would expect and the actual behavior you encountered.
- Please provide as much context as possible and describe the *reproduction steps* that someone else can follow to recreate the issue on their own. This usually includes your code. For good bug reports you should isolate the problem and create a reduced test case.
- Provide the information detailed in the [previous section](#before-submitting-a-report).

Once it's filed:

- We will label the issue accordingly.
- We will try to reproduce the issue with your provided steps. If there are no reproduction steps or no obvious way to reproduce the issue, the team will ask you for those steps and mark the issue as `needs-repro`. Bugs with the `needs-repro` tag will not be addressed until they are reproduced.
- If we are able to reproduce the issue, it will be marked `needs-fix`, as well as possibly other tags (such as `critical`), and the fix will be left to be [implemented by someone](#your-first-code-contribution).

<!-- TODO: Create issue template for bugs and errors that can be used as a guide and that defines the structure of the information to be included. Post reference to it here. -->

### Suggesting Enhancements

This section guides you through submitting an enhancement suggestion for Jink, **including completely new features and minor improvements to existing functionality**. Following these guidelines will help maintainers and the community to understand your suggestion and find related suggestions.

#### Before Submitting an Enhancement

- Make sure that you are using the latest version.
<!-- - Read the [documentation]() carefully and find out if the functionality is already covered, maybe by an individual configuration.-->
- Perform a [search](https://github.com/jink-lang/jink/labels/enhancement) to see if the enhancement has already been suggested. If it has, add a comment to the existing issue instead of opening a new one.
- Find out whether your idea fits with the scope and aims of the project. It's up to you to make a strong case to convince us of the merits of this feature. Keep in mind that we want features that will be useful to the majority of users and not just a small subset. If you are unsure, you can open a discussion in Jacob's project [Discord server](https://discord.gg/cWzcQz2) or in the issue itself.

#### How Do I Submit a Good Enhancement Suggestion?

Enhancement suggestions are tracked as [GitHub issues](https://github.com/jink-lang/jink/issues).

- Use a **clear and descriptive title** for the issue to identify the suggestion.
- Provide a **step-by-step description of the suggested enhancement** in as many details as possible.
- **Describe the current behavior** and **explain which behavior you expected to see instead** if applicable and why. At this point you can also explain which alternatives do not work for you.
- You may want to include **screenshots** or **visual cues** which help you demonstrate the steps or the utility behind your suggestion.
- **Explain why this enhancement would be useful** to Jink, and to Jink users. Feel free to make comparisons with other languages that solved a problem better and which could serve as inspiration.

<!-- TODO: Create an issue template for enhancement suggestions that can be used as a guide and that defines the structure of the information to be included. Post reference to it here. -->

### Your First Contribution

So you've decided to contribute to Jink? That's great! You'll be helping make the project better for everyone, even if you're new to open source or if it is something small. This section will guide you through your first contribution to the project.

#### Setup

Jink is built on Rust and you will need it installed to build the compiler. You can install Rust by following the instructions on the [Rust website](https://www.rust-lang.org/tools/install). You will also need to have Git. You can install Git by following the instructions on the [Git website](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git).

#### Getting started

To get started on your first contribution, you can follow these steps:

- Fork the repository by clicking the `Fork` button on the top right of the repository page and clone your fork to your local machine. If you are new to Git, you can follow the instructions on the [GitHub Guides](https://guides.github.com/activities/forking/).
- Create a new branch for your contribution. You can do this by running `git checkout -b branch-name` in your terminal. Make sure to replace `branch-name` with a descriptive name for your branch.
- Make your changes to the codebase. You can use your preferred text editor or IDE. We recommend [Visual Studio Code](https://code.visualstudio.com/) with the [rust-analyzer](https://marketplace.visualstudio.com/items?itemName=rust-lang.rust-analyzer) and of course [Jink language](https://marketplace.visualstudio.com/items?itemName=jink-lang.jink) extensions.
- Make absolutely sure to follow the [code](#code-style) and [commit message](#commit-messages) style guides when making your changes.
- Run the tests to make sure your changes didn't break anything. You can run the tests by running `cargo test` in your terminal. All tests should pass before you open a pull request.
- Once you are happy with your changes, commit and push them to your branch. A good commit message should be descriptive and explain what changes you made.
- Open a pull request by clicking the `New pull request` button on the repository page. Make sure to provide a detailed description of your changes and why they are necessary. You can also reference any related issues to what you are contributing.
- Once your pull request is opened, a maintainer will review it and provide feedback. You may need to make additional changes based on the feedback you receive.

Once everything is in order and your pull request is approved, it will be merged and your contribution will be part of the project! Congratulations! 🎉

<!-- ### Improving The Documentation -->
<!-- TODO
Updating, improving and correcting the documentation

-->

## Style Guide

### Code Style

We ask that you adhere to the existing code style when making contributions to the project. This includes:

- Using 2 spaces for indentation
- Using snake_case for variable and function names
- Using SCREAMING_SNAKE_CASE for constants
- Using PascalCase for type names

For more specific styling examples, please refer to the existing codebase. We recommend against using a linter or formatter, as they are often very strict and cause more harm than good. Instead, use your best judgment and try to match the existing style as closely as possible.

### Commit Messages

When making commits to the project, be as descriptive as possible in your commit messages. A good commit message should:

- Be descriptive and explain what changes were made
- Be concise and to the point
- Apply to a single change or set of related changes

For example, good commit messages might look like this:

```
Added lexer support for string templates
```
```
Added wiki docs for the new string template feature
```

This explains what is being changed and to what, without being overly verbose or too brief.

A bad commit message might look like this:

```
Fixed bug
```

You need not overthink it, do try to provide as much context as possible in your commit messages, and if necessary, provide additional context in the body of the commit message and/or make multiple commits to separate unrelated changes. These things will help maintainers and other contributors understand the specific changes you made.

<!-- ## Join The Project Team -->
<!-- TODO -->
