.PHONY: build
build:
	prmd combine -m meta.yaml schemata > schema.json
	prmd verify schema.json > /dev/null
	prmd doc schema.json > schema.md
