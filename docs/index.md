# Belka Templates

## What are templates?

Templates are just a way of seperating out the layout of a page from its particular contents. I want to show the same home page in my application written with the [Belka Gemini Server](http://gordonguthrie.github.io/belka) but using the users name in a particular place.

Templates are a simple way to split out the layout of the page with some parameters in place of the text that changes.

Belka Templates use [Dactyl](https://github.com/basho/dactyl) as the smallest template engine.

Belka Templates will support multiple web sites. The server picks up a root directory from the system config (see the example file). In that directory there should be a directory for each website that is being served on the Belka server and each application should put its own templates in there.

 <div>
 {% for item in site.data.contents.toc %}
     <h3>{{ item.title }}</h3>
       <ul>
         {% for entry in item.subfolderitems %}
           <li><a href="{{ entry.url }}">{{ entry.page }}</a></li>
         {% endfor %}
       </ul>
   {% endfor %}
 </div>
