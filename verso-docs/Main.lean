/-
TetraGrayer Documentation Site Generator

Uses Verso's literate Lean feature for proper syntax highlighting.
-/
import VersoBlog
import TetraGrayerDocs
import TetraGrayerDocs.LitParticle
import TetraGrayerDocs.LitCode

open Verso Genre Blog Site Syntax
open Output Html Template Theme

-- Import literate Lean modules with proper syntax highlighting
-- Uses `set_option doc.verso true` for SubVerso highlighting
literate_page particleDocs from TetraGrayerDocs.LitParticle in "." as "Particle Types"
literate_page codeDocs from TetraGrayerDocs.LitCode in "." as "Code Examples"

def theme : Theme := { Theme.default with
  primaryTemplate := do
    return {{
      <html>
        <head>
          <meta charset="utf-8"/>
          <meta name="viewport" content="width=device-width, initial-scale=1"/>
          <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/sakura.css/css/sakura.css" type="text/css"/>
          <title>{{ (← param (α := String) "title") }} " — TetraGrayer"</title>
          {{← builtinHeader }}
          <style>
            "body { max-width: 900px; }"
            "img { max-width: 100%; height: auto; border-radius: 8px; margin: 1em 0; box-shadow: 0 2px 8px rgba(0,0,0,0.1); }"
            "pre { background: #f5f5f5; padding: 1em; border-radius: 8px; overflow-x: auto; }"
            "code { font-family: 'SF Mono', 'Consolas', monospace; }"
            "table { width: 100%; border-collapse: collapse; }"
            "th, td { padding: 0.5em; border: 1px solid #ddd; }"
            ".nav { display: flex; gap: 2em; margin: 1em 0; }"
            ".nav a { color: #1d4ed8; }"
            "h1 { color: #1e293b; }"
          </style>
        </head>
        <body>
          <header>
            <h1><a href="." style="text-decoration: none; color: inherit;">"TetraGrayer"</a></h1>
            <div class="nav">
              {{ ← topNav }}
            </div>
          </header>
          <main>
            {{ (← param "content") }}
          </main>
          <footer style="margin-top: 3em; padding-top: 1em; border-top: 1px solid #ddd; color: #666;">
            <p>
              "Built with "
              <a href="https://github.com/leanprover/verso">"Verso"</a>
              " and "
              <a href="https://leanprover.github.io/">"Lean 4"</a>
            </p>
          </footer>
        </body>
      </html>
    }}
}

def tetraGrayerSite : Site := site TetraGrayerDocs.Front /
  static "static" ← "static"
  "gallery" TetraGrayerDocs.Gallery
  "code" codeDocs
  "particle" particleDocs

def main := blogMain theme tetraGrayerSite
