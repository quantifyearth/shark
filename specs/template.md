---
path_prefix: /var/
project_ids:
  - 1234
  - 4321
---

With templating support, Shark markdown files can support a limited amount of flexibility
in how they are interpretted at runtime.

~~~shark-build:gdal-env
((from ghcr.io/osgeo/gdal:ubuntu-small-3.6.4))
~~~

The syntax is light-weight and similar to Github Actions. Simply add some frontmatter for each
variable and quote them with `%{project_ids}`. Separate nested varibales with a dot.

~~~shark-run:gdal-env
$ echo %{project_ids} > %{path_prefix}%{project_ids}.txt
$ cat %{path_prefix}%{project_ids}.txt
~~~