var  width = 1000;
     height = 1000;
     font_size = 15,
     text_padding = 3;

var svg = d3.select("body").append("svg")
    .attr("width", width)
    .attr("height", height);

var color = d3.scale.category20();

var force = d3.layout.force()
    .charge(-3000)
    .gravity(0.05)
    .friction(0.1)
    .linkStrength(0.4)
    .size([width, height]);

function update_force_layout(nodes_, links_){
    force.nodes(nodes_).links(links_).start();

    svg.selectAll("line.link").remove();
    svg.selectAll("g.group").remove();

    var link = svg.selectAll("line.link")
                  .data(links_)
                  .enter().append("line")
                  .attr("class", "link");

    var group = svg.selectAll("g.group")
                   .data(nodes_)
                   .enter().append("g")
                   .attr("class", "group")
                   .on('mouseover', function(d){
                       var inner_text = [d.name, '>'].concat(d.columns.map(function(e){ return e.name + '::' + e.type }));
                       var tooltip = svg.select('.tooltip')
                                        .append('g')
                                        .attr('class', 'tooltip');

                       var text = tooltip.selectAll('text')
                                         .data(inner_text)
                                         .append('text')
                                         .attr('x', 0) // d.x)
                                         .attr('y', 0) // function(e, i){ return d.y + (i + 1) * font_size })
                                         .attr('fill', '#f00')
                                         .text(function(e){ return d });

                       var text_box_width  = d3.max(text[0].map(function(e){ return text_padding + e.getBBox().width; }));
                       var text_box_height = d3.sum(text[0].map(function(e){ return text_padding + e.getBBox().height; }));

                       tooltip.selectAll('text')
                              .insert("rect", "text")
                              .attr("width", function(d){  return text_box_width })
                              .attr("height", function(d){ return text_box_height })
                              .attr("rx", 3)
                              .attr("ry", 3)
                              .style("fill", "#ffa")
                              .style("opacity", 0.1)
                              .attr("x", 0)
                              .attr("y", 0)
                              .style("stroke-width", "0");
                   })
                   .on('mouseout', function(d){
                      // d3.selectAll('.tooltip').remove();
                   })
                   .each(function(d){
                      var text = d3.select(this).selectAll("text")
                                   .data([getOnlyUpperCase(d.name)])
                                   .enter()
                                   .append("text")
                                   .attr("y", font_size * 0.5)
                                   .attr("fill", "#000")
                                   .attr("font-size", font_size)
                                   .attr("text-anchor", "middle")
                                   .text(function(d){ return d });

                      var text_box_width  = d3.max(text[0].map(function(e){ return text_padding + e.getBBox().width; }));
                      var text_box_height = d3.sum(text[0].map(function(e){ return text_padding + e.getBBox().height; }));

                      d3.select(this)
                        .insert("rect", "text")
                        .attr("width", function(d){  return text_box_width })
                        .attr("height", function(d){ return text_box_height })
                        .attr("rx", 3)
                        .attr("ry", 3)
                        .style("fill", function(d){ return color(d.name) })
                        .attr("x", (-1) * text_box_width * 0.5)
                        .attr("y", (-1) * font_size * 0.5)
                        .style("stroke-width", "0")
                        .call(force.drag);
                   });

    link.filter(function(d){ return d.disabled }).remove();
    group.filter(function(d){ return d.disabled }).remove();

    force.on("tick", function() {
      link.attr("x1", function(d) { return d.source.x; })
          .attr("y1", function(d) { return d.source.y; })
          .attr("x2", function(d) { return d.target.x; })
          .attr("y2", function(d) { return d.target.y; });

      group.attr("transform", function(d) { return "translate(" + d.x + ", " + d.y + ")"; });
    });
} // update_force_layout end

var start_node_index = graph.nodes.map(function(n){ return n.name }).indexOf("Organization");
var tree_links = getTree(graph.links, d3.range(graph.nodes.length), start_node_index);

console.log(["graph.nodes", graph.nodes]);
console.log(["graph.links", graph.links]);
console.log(["start_node_index", start_node_index]);

console.log(["tree_links", tree_links]);
console.log(["tree_links.slice(1, tree_links.length)", tree_links.slice(1, tree_links.length)]);

update_force_layout(graph.nodes, tree_links.slice(1, tree_links.length));

d3.select("#send")
  .on("click", function(){
    var pattern = new RegExp(d3.select("#node-filter").property("value"), 'i');
    graph.nodes.forEach(function(d){
      d.disabled = !d.name.match(pattern);
    });
    graph.links.forEach(function(d){
      d.disabled = !d.source.name.match(pattern) || !d.target.name.match(pattern);
    });
    update_force_layout(graph.nodes, graph.links);
  });

d3.select("#start")
  .on("click", function(){
    force.start();
  })

d3.select("#stop")
  .on("click", function(){
    force.stop();
  })

// == helper functions =================================
function getOnlyUpperCase(str){
  return str.split("").filter(function(e){ return e.toUpperCase() == e }).join("");
}

// console.log(getTree([["b", "a"], ["b", "c"]], ["a", "b", "c", "e"], "b")); //=> [["b", null], ["a", "b"], ["b", "c"]]
// links :: [{source: Int, target: Int}]
// vertex :: [Int]
// v0 :: Int
// 戻り値 :: [(Int, Maybe Int)]
function getTree(links, vertex, v0){
  var distance = {},
      last_v = {},
      result = [];
      remainder = vertex;

  vertex.forEach(function(v){
    distance[v] = (v == v0) ? 0 : Infinity;
    last_v[v] = -1;
  });

  while(remainder.length > 0){
    u = remainder.sort(function(v){ return distance[v] })[0] // 最小となる点
    result.push([u, last_v[u]])
    remainder = remainder.filter(function(e){ return e != u });
    neibor(remainder, links, u).forEach(function(v){
      if (distance[v] > distance[u] + 1){
        distance[v] = distance[u] + 1
        last_v[v] = u  // betterな隣接点
      }
    })
  }

  return result.map(function(e){ return {source: e[0], target: e[1]} });
}

// nodes :: [Int]
// links :: [{source: Int, target: Int}]
// 戻り値 :: [Int]
function neibor(nodes, links, p){
  var arr = links.filter(function(e){ return (e.source == p || e.target == p) }).map(function(e){ return d3.values(e) });
  var neibor = d3.set(d3.merge(arr)).values().map(function(e){ return +e }).filter(function(e){ return (0 <= nodes.map(function(e_){ return +e_ }).indexOf(e)) });
  return neibor;
}
