#include <Python.h>

int yaml_indent_level(int spaces, int tab_width) {
    if (tab_width <= 0) {
        PyErr_SetString(PyExc_ValueError, "tab_width must be positive");
        return 0;
    }
    return spaces / tab_width;
}
