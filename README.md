# Google+ Browser

This is code to browse your Google+ Archive. Look at posts you made
and decide what you want to do with them. Currently, the only actions
implemented are:

1. scroll through the archive using `SPC`
1. upload post to an [Oddmuse](https://oddmuse.org/) wiki using `C-c C-c`
2. delete post from the archive using `d`

Dependencies:

1. `markdown-mode` from MELPA
2. `html-to-markdown` from MELPA
3. `oddmuse-curl` if you want to post to an Oddmuse wiki
   ([https://alexschroeder.ch/cgit/oddmuse-curl/about/ source])

This is what the UI looks like:

![Screenshot](screenshot.png)

When looking at pages before posting, I find that `visual-line-mode`
helps. So perhaps you should enable it, too?

On my blog, almost all pages are *tagged*. I added `C-c C-t` to
simplify adding tags to pages. Add your favorite tags to this
variable:

```
(setq oddmuse-tags '("RPG" "Old School" "RSP" "Maps"
		     "Software" "Copyright" "Social Media"
		     "Administration" "Programming" "Mastodon" "Trunk"
		     "Books" "Movies" "Music" "Life" "Podcast" "Gridmapper"
		     "Web" "Oddmuse" "Wikis" "Blogs" "Text Mapper" "Hex Describe"
		     "Switzerland"))
```

If you want to change what `C-c C-c` does, you should take a look at
`gplus-post-buffer`. Perhaps we can add an option that determines your
target system and start collection a few functions to do whatever is
needed? I realize not many people will be running an Oddmuse wiki.
