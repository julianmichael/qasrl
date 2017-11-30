# qasrl-crowdsourcing

Repository for QA-SRL tools, particularly for interaction and crowdsourcing.

## Contents

There is currently one project in this repository.

 * `qasrl`: General tools for validating, interpreting, and autocompleting QA-SRL.
 
Soon this will also be home to `qasrl-crowd`, which will contain the tools
for setting up a QA-SRL crowdsourcing pipeline on Mechanical Turk.

## Usage

Since I have not published this code on Maven or anything, it is easiest to use it from source,
for example, as a submodule in your project.
Clone it to your machine and run `scripts/setup.sh`.
This will download the `nlpdata` dependency and publish it locally to your Ivy cache so this will compile.
It will also prompt you to download a Wiktionary dataset, which will be placed at `datasets/wiktionary`.
This will be necessary to run the QA-SRL validation and autocomplete.

Once that is done, to get an idea of how to use the tools, try running `scripts/run_autocomplete_example.sh`.
This will throw you into a REPL where you may type prefixes to questions (or full questions)
and get the autocomplete feedback displayed to you when you press Enter.

Then, if you wish to use the autocomplete or other functionality in your own code,
take a look at `scripts/autocomplete_example.scala`, which was running in the previous step, for example usage.

Finally, to use this project in yours, either add it as a source dependency or run
`sbt publishLocal` and add it as a managed dependency in your desired project.
