"""
color_scheme_matcher.

Licensed under MIT.

Copyright (C) 2012  Andrew Gibson <agibsonsw@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
documentation files (the "Software"), to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and
to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of
the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO
THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

---------------------

Original code has been heavily modifed by Isaac Muse <isaacmuse@gmail.com> for the ExportHtml project.
Algorithm has been split out into a separate library and been enhanced with a number of features.
"""
from __future__ import absolute_import
import sublime
import re
from .rgba import RGBA
from os import path
from collections import namedtuple
from plistlib import readPlistFromBytes


class SchemeColors(
    namedtuple(
        'SchemeColors',
        ['fg', 'fg_simulated', 'bg', "bg_simulated", "style", "fg_selector", "bg_selector", "style_selectors"],
        verbose=False
    )
):
    """SchemeColors."""


class SchemeSelectors(namedtuple('SchemeSelectors', ['name', 'scope'], verbose=False)):
    """SchemeSelectors."""


def sublime_format_path(pth):
    """Format path for sublime internal use."""

    m = re.match(r"^([A-Za-z]{1}):(?:/|\\)(.*)", pth)
    if sublime.platform() == "windows" and m is not None:
        pth = m.group(1) + "/" + m.group(2)
    return pth.replace("\\", "/")


class ColorSchemeMatcher(object):
    """Determine color scheme colors and style for text in a Sublime view buffer."""

    def __init__(self, scheme_file, color_filter=None):
        """Initialize."""
        if color_filter is None:
            color_filter = self.filter
        self.color_scheme = path.normpath(scheme_file)
        self.scheme_file = path.basename(self.color_scheme)
        self.plist_file = color_filter(
            readPlistFromBytes(
                re.sub(
                    br"^[\r\n\s]*<!--[\s\S]*?-->[\s\r\n]*|<!--[\s\S]*?-->", b'',
                    sublime.load_binary_resource(sublime_format_path(self.color_scheme))
                )
            )
        )
        self.scheme_file = scheme_file
        self.matched = {}

        self.parse_scheme()

    def filter(self, plist):
        """Dummy filter call that does nothing."""

        return plist

    def parse_scheme(self):
        """Parse the color scheme."""

        color_settings = {}
        for item in self.plist_file["settings"]:
            if item.get('scope', None) is None and item.get('name', None) is None:
                color_settings = item["settings"]
                break

        # Get general theme colors from color scheme file
        self.bground, self.bground_sim = self.strip_color(
            color_settings.get("background", '#FFFFFF'), simple_strip=True, bg=True
        )

        self.fground, self.fground_sim = self.strip_color(color_settings.get("foreground", '#000000'))
        self.sbground = self.strip_color(color_settings.get("selection", self.fground))[0]
        self.sbground_sim = self.strip_color(color_settings.get("selection", self.fground_sim))[1]
        self.sfground, self.sfground_sim = self.strip_color(color_settings.get("selectionForeground", None))

        # Create scope colors mapping from color scheme file
        self.colors = {}
        for item in self.plist_file["settings"]:
            name = item.get('name', '')
            scope = item.get('scope', None)
            color = None
            style = []
            if 'settings' in item and scope is not None:
                color = item['settings'].get('foreground', None)
                bgcolor = item['settings'].get('background', None)
                if 'fontStyle' in item['settings']:
                    for s in item['settings']['fontStyle'].split(' '):
                        if s == "bold" or s == "italic":  # or s == "underline":
                            style.append(s)

            if scope is not None and (color is not None or bgcolor is not None):
                fg, fg_sim = self.strip_color(color)
                bg, bg_sim = self.strip_color(bgcolor, bg=True)
                self.colors[scope] = {
                    "name": name,
                    "scope": scope,
                    "color": fg,
                    "color_simulated": fg_sim,
                    "bgcolor": bg,
                    "bgcolor_simulated": bg_sim,
                    "style": style
                }

    def strip_color(self, color, simple_strip=False, bg=False):
        """
        Strip transparency from the color value.

        Transparency can be stripped in one of two ways:
            - Simply mask off the alpha channel.
            - Apply the alpha channel to the color essential getting the color seen by the eye.
        """

        if color is None or color.strip() == "":
            return None, None

        rgba = RGBA(color.replace(" ", ""))
        if not simple_strip:
            rgba.apply_alpha(self.bground_sim if self.bground_sim != "" else "#FFFFFF")

        return color, rgba.get_rgb()

    def get_general_colors(self, simulate_transparency=False):
        """
        Get the core colors (background, foreground) for the view and gutter.

        Get the visible look of the color by simulated transparency if requrested.
        """
        if simulate_transparency:
            return self.bground_sim, self.fground_sim, self.sbground_sim, self.sfground_sim
        else:
            return self.bground, self.fground, self.sbground, self.sfground

    def guess_color(self, view, pt, scope_key):
        """Guess the colors and style of the text for the given Sublime view pt."""

        color = self.fground
        color_sim = self.fground_sim
        bgcolor = None
        bgcolor_sim = None
        style = set([])
        color_selector = SchemeSelectors("foreground", "foreground")
        bg_selector = SchemeSelectors("background", "background")
        style_selectors = {"bold": SchemeSelectors("", ""), "italic": SchemeSelectors("", "")}
        if scope_key in self.matched:
            color = self.matched[scope_key]["color"]
            color_sim = self.matched[scope_key]["color_simulated"]
            style = self.matched[scope_key]["style"]
            bgcolor = self.matched[scope_key]["bgcolor"]
            bgcolor_sim = self.matched[scope_key]["bgcolor_simulated"]
            selectors = self.matched[scope_key]["selectors"]
            color_selector = selectors["color"]
            bg_selector = selectors["background"]
            style_selectors = selectors["style"]
        else:
            best_match_bg = 0
            best_match_fg = 0
            best_match_style = 0
            for key in self.colors:
                match = view.score_selector(pt, key)
                if self.colors[key]["color"] is not None and match > best_match_fg:
                    best_match_fg = match
                    color = self.colors[key]["color"]
                    color_sim = self.colors[key]["color_simulated"]
                    color_selector = SchemeSelectors(self.colors[key]["name"], self.colors[key]["scope"])
                if self.colors[key]["style"] is not None and match > best_match_style:
                    best_match_style = match
                    for s in self.colors[key]["style"]:
                        style.add(s)
                        if s == "bold":
                            style_selectors["bold"] = SchemeSelectors(
                                self.colors[key]["name"], self.colors[key]["scope"]
                            )
                        elif s == "italic":
                            style_selectors["italic"] = SchemeSelectors(
                                self.colors[key]["name"], self.colors[key]["scope"]
                            )
                if self.colors[key]["bgcolor"] is not None and match > best_match_bg:
                    best_match_bg = match
                    bgcolor = self.colors[key]["bgcolor"]
                    bgcolor_sim = self.colors[key]["bgcolor_simulated"]
                    bg_selector = SchemeSelectors(self.colors[key]["name"], self.colors[key]["scope"])
            self.matched[scope_key] = {
                "color": color,
                "bgcolor": bgcolor,
                "color_simulated": color_sim,
                "bgcolor_simulated": bgcolor_sim,
                "style": style,
                "selectors": {
                    "color": color_selector,
                    "background": bg_selector,
                    "style": style_selectors
                }
            }
        if len(style) == 0:
            style = ""
        else:
            style = ' '.join(style)
        return SchemeColors(
            color, color_sim, bgcolor, bgcolor_sim, style,
            color_selector, bg_selector, style_selectors
        )
