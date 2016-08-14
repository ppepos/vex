import simuvex
import archinfo
import jinja2
import os
import sys
import subprocess

filename = 'pub/libvex_guest_%s.h' % sys.argv[1]
struct = 'VexGuest%sState' % sys.argv[1].upper()

initial_thing = simuvex.s_type.parse_defns(open('pub/libvex_basictypes.h').read() + open(filename).read())
arch_struct = initial_thing[struct].with_arch(archinfo.get_host_arch())
field_names = arch_struct.fields.keys()
field_sizes = [ty.size/8 for ty in arch_struct.fields.itervalues()]
register_names = [field[6:].lower() if field.startswith('guest_') else field.lower() for field in field_names]

c_template = open('find_offsets.c.template').read()
templater = jinja2.Template(c_template)
c_file = templater.render(filename=filename, structure=struct, fields=field_names)
open('find_offsets.c', 'w').write(c_file)
if os.system('gcc find_offsets.c -o find_offsets') != 0:
    sys.exit(1)

offset_data, _ = subprocess.Popen(['./find_offsets'], stdout=subprocess.PIPE).communicate()
field_offsets = map(int, offset_data.split())
field_data = sorted(zip(field_offsets, register_names, field_sizes))

# here we go
final_template = """
    register_names = {
{% for offset, name, size in field_data %}        {{offset}}: '{{name}}',
{% endfor %}    }

    registers = {
{% for offset, name, size in field_data %}        '{{name}}': ({{offset}}, {{size}}),
{% endfor %}    }
"""

templater = jinja2.Template(final_template)
print templater.render(field_data=field_data)
