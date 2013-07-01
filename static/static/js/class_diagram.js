var  width = 1000;
     height = 1000;
     font_size = 15,
     text_padding = 3;

var svg = d3.select("body").append("svg")
    .attr("width", width)
    .attr("height", height);

var color = d3.scale.category20();

var force = d3.layout.force()
    .charge(-1000)
    .linkDistance(400)
    .gravity(.05)
    .distance(200)
    .friction(0.1)
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
                       svg.selectAll('text.tooltip')
                         .data(inner_text)
                         .enter()
                         .append('text')
                         .attr('class', 'tooltip')
                         .attr('x', d.x)
                         .attr('y', function(e, i){ return d.y + (i + 1) * 30 })
                         .attr('fill', '#f00')
                         .text(function(d){ return d });
                   })
                   .on('mouseout', function(d){
                      d3.selectAll('text.tooltip').remove();
                   })
                   .each(function(d){
                      var text = d3.select(this).selectAll("text")
                                   .data([getOnlyUpperCase(d.name)])
                                   .enter()
                                   .append("text")
                                   .attr("y", function(d, i){
                                           return (text_padding + font_size) * (i+1);
                                   })
                                   .attr("fill", "#000")
                                   .attr("font-family", "impact")
                                   .attr("font-size", font_size)
                                   .text(function(d){ return d });

                      var text_box_width  = d3.max(text[0].map(function(e){ return text_padding + e.getBBox().width; }));
                      var text_box_height = d3.sum(text[0].map(function(e){ return text_padding + e.getBBox().height; }));

                      d3.select(this)
                        .insert("rect", "text")
                        .attr("width", function(d){  return text_box_width })
                        .attr("height", function(d){ return text_box_height })
                        .attr("rx", 3)
                        .attr("ry", 3)
                        .style("fill", "#ccc")
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

update_force_layout(graph.nodes, graph.links);

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

// helper functions
function getOnlyUpperCase(str){
  return str.split("").filter(function(e){ return e.toUpperCase() == e }).join("");
}
