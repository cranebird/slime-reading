
.SUFFIXES: .puml .png
.puml.png:
	GRAPHVIZ_DOT=/opt/local/bin/dot java -jar plantuml.jar $<

images = sequence.png seq-swank-boot.png seq-C-c-C-m.png deploy.png seq-net-filter.png comp-slime.png comp-swank.png comp-swank-backend.png seq-slime-boot.png

all: $(images)

