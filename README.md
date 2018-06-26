# qasrl-crowdsourcing

Repository for QA-SRL tools, particularly for interaction and crowdsourcing.

## Contents

There are currently three projects in this repository.

 * `qasrl`: General tools for validating, interpreting, and autocompleting QA-SRL.
 * `qasrl-crowd`: UI and server code for for crowdsourcing QA-SRL data on Mechanical Turk.
 * `qasrl-crowd-example`: Standalone Mill project demonstrating how to use the crowdsourcing pipeline for your own data.

## Usage

This project is built with [Mill](http://lihaoyi.com/mill).
I haven't published this code on Maven yet, so the recommended way to use it is to publish locally
from source, i.e., clone this repository and run `mill __.publishLocal` in the repository root.
Once that is done, you can play around with the example project in `qasrl-crowd-example`, possibly
copying it elsewhere and modifying it to fit your needs.

If you just want the base QA-SRL tools (e.g., autocomplete), do this then include
```
  libraryDependencies += "org.julianmichael" %%% "nlpdata" % "0.1.0"
  libraryDependencies += "org.julianmichael" %%% "qasrl" % "0.1.0-SNAPSHOT"
```
in your sbt project settings, or 
```
  "org.julianmichael::nlpdata::0.1.0",
  ivy"org.julianmichael::qasrl::0.1.0-SNAPSHOT"
```
in your Mill `ivyDeps`.
You then need to download the Wiktionary data and place it somewhere accessible by your project
so you can inflect verbs.

### Running the example project

After locally publishing the `qasrl` and `qasrl-crowd` projects, run
`qasrl-crowd-example/scripts/setup.sh`.
This will prompt you to download the Wiktionary dataset, which you should do.
Then run `qasrl-crowd-example/scripts/run_crowd_example.sh`.
Currently, you need to be using the latest nightly build of Mill,
though any version after 0.2.3 should work.

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

 * [`scripts/crowd_example.scala`](https://github.com/julianmichael/qasrl-crowdsourcing/blob/master/scripts/crowd_example.scala)
   is what you run on the SBT console to get started.
 * That creates an [`AnnotationSetup`](https://github.com/julianmichael/qasrl-crowdsourcing/blob/master/qasrl-crowd-example/jvm/src/main/scala/example/AnnotationSetup.scala) object defined in `qasrl-crowd-example`,
   which assembles the various data and resources needed for the crowdsourcing pipeline.
 * That creates a [`QASRLAnnotationPipeline`](https://github.com/julianmichael/qasrl-crowdsourcing/blob/master/qasrl-crowd/jvm/src/main/scala/qasrl/crowd/QASRLAnnotationPipeline.scala) object,
   which creates the web services and interfaces with MTurk to upload and download data and assess workers.
 * Finally, telling the `QASRLAnnotationPipeline` object to `start()` will start the crowdsourcing task.
 
Again, see the [instructions in the QAMR project](https://github.com/uwnlp/qamr/tree/master/code) for a more detailed account (though it is very slightly out of date).
