#* Imports
import pycook.recipes.pip as pip
from pycook.recipes.pip import clean, sdist

#* Recipes
def pip_reinstall(recipe):
    res = []
    if pip.package_installed_p("pomodoro-indicator"):
        res += [pip.uninstall("pomodoro-indicator")]
    res += [pip.install(".")]
    return res

def publish(recipe):
    return sdist(recipe) + ["twine upload dist/*"]

def sdist(recipe):
    return [
        "rm -rf dist/",
        "python setup.py sdist"]
