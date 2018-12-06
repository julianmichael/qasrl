# qasrl-crowdsourcing

Repository for QA-SRL tools, particularly for interaction and crowdsourcing.

## Contents

There are currently three projects in this repository.

 * `qasrl`: General tools for validating, interpreting, and autocompleting QA-SRL.
 * `qasrl-crowd`: UI and server code for for crowdsourcing QA-SRL data on Mechanical Turk.
 * `qasrl-crowd-example`: Standalone Mill project demonstrating how to use the crowdsourcing pipeline for your own data.

## Usage

Relevant imports for this project are as follows:
```
  libraryDependencies += "org.julianmichael" %%% "nlpdata" % "0.2.0"
  libraryDependencies += "org.julianmichael" %%% "qasrl" % "0.1.0"
  libraryDependencies += "org.julianmichael" %%% "qasrl-crowd" % "0.1.0"
```
in your sbt project settings, or 
```
  ivy"org.julianmichael::nlpdata::0.2.0",
  ivy"org.julianmichael::qasrl::0.1.0",
  ivy"org.julianmichael::qasrl-crowd::0.1.0"
```
in your Mill `ivyDeps`.
You then need to download the Wiktionary data and place it somewhere accessible by your project
so you can inflect verbs.

To run the scripts and example project in this repo, you need
[Mill](http://www.lihaoyi.com/mill/index.html) version 0.2.5 or later.

### Running the example project

Run `qasrl-crowd-example/scripts/setup.sh`.
This will prompt you to download the Wiktionary dataset, which you should do.
Then run `qasrl-crowd-example/scripts/run_crowd_example.sh`.

### Autocomplete

To get an idea of how the autocomplete functionality works, try running `scripts/run_autocomplete_example.sh`.
This will throw you into a REPL where you may type prefixes to questions (or full questions)
and get the autocomplete feedback displayed to you when you press Enter.

If you wish to use the autocomplete or other functionality in your own code,
take a look at `scripts/autocomplete_example.scala`, which was running in the previous step, for example usage.

### Crowdsourcing

To start up the crowdsourcing pipeline and see a preview of the task UI, run `scripts/run_crowd_example.sh` and go to
`localhost:8888/task/generation/preview` in your browser.
To understand how to set everything up so it works on MTurk,
adapt the instructions [here](https://github.com/uwnlp/qamr/tree/master/code)
to the local code. To trace the main entry points:

 * [`scripts/crowd_example.scala`](https://github.com/julianmichael/qasrl-crowdsourcing/blob/master/qasrl-crowd-example/scripts/crowd_example.scala)
   is what you run on the SBT console to get started.
 * That creates an [`AnnotationSetup`](https://github.com/julianmichael/qasrl-crowdsourcing/blob/master/qasrl-crowd-example/example/src-jvm/example/AnnotationSetup.scala) object defined in `qasrl-crowd-example`,
   which assembles the various data and resources needed for the crowdsourcing pipeline.
 * That creates a [`QASRLAnnotationPipeline`](https://github.com/julianmichael/qasrl-crowdsourcing/blob/master/qasrl-crowd/src-jvm/qasrl/crowd/QASRLAnnotationPipeline.scala) object,
   which creates the web services and interfaces with MTurk to upload and download data and assess workers.
 * Finally, telling the `QASRLAnnotationPipeline` object to `start()` will start the crowdsourcing task.
 
Again, see the [instructions in the QAMR project](https://github.com/uwnlp/qamr/tree/master/code) for a more detailed account (though it is very slightly out of date).
