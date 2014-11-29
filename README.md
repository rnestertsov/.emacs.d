# Emacs config
Almost all configuration is splitted into logical pieces and placed under ./conf directory. 
Such files then called from init.el.

## Org mode and external application
I am using following applications together with org-babel model to produce various technical documentation:
- [Ditaa](http://ditaa.sourceforge.net/)
- [PlantUML](http://plantuml.sourceforge.net/)
- [Graphviz](http://www.graphviz.org/)

"org-mode" and "plantuml-mode" require path to "plantuml" and "ditaa" application to be specified. This is configured in 2 places:

conf/conf-org.el:
```elisp
(setq org-ditaa-jar-path "c:/bin/ditaa0_9.jar")
(setq org-plantuml-jar-path "c:/bin/plantuml.jar")
```

vendor/plantuml-mode.el
```elisp
(defvar plantuml-jar-path (expand-file-name "c:/bin/plantuml.jar"))
```

## Spellchecking
I use [GNU Aspell](http://aspell.net/) for spellchecking. It's configured in "conf-spellcheck.el".
