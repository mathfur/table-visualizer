var  width = 960,
     height = 500,
     font_size = 15,
     text_padding = 3;

var color = d3.scale.category20();

var force = d3.layout.force()
    .charge(-1000)
    .linkDistance(120)
    .size([width, height]);

var svg = d3.select("body").append("svg")
    .attr("width", width)
    .attr("height", height);

console.log(graph.nodes);
console.log(graph.links);

force.nodes(graph.nodes).links(graph.links).start();

var link = svg.selectAll(".link").data(graph.links).enter().append("line").attr("class", "link");

var group = svg.selectAll(".group")
               .data(graph.nodes)
               .enter().append("g")
               .on('mouseover', function(d){
                   console.log(d.x);
                   console.log(d.y);
                   // var x = parseFloat(d3.select(this)[0][0].attr('x'));
                   // var y = parseFloat(d3.select(this)[0][0].attr('y'));
                   var inner_text = [d.name, '>'].concat(d.columns.map(function(e){ return e.name + '::' + e.type }));
                   console.log(inner_text);
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
                               .data(getOnlyUpperCase(d.name))
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

force.on("tick", function() {
  link.attr("x1", function(d) { return d.source.x; })
      .attr("y1", function(d) { return d.source.y; })
      .attr("x2", function(d) { return d.target.x; })
      .attr("y2", function(d) { return d.target.y; });

  group.attr("transform", function(d) { return "translate(" + d.x + ", " + d.y + ")"; });
});

// helper functions
function getOnlyUpperCase(str){
  return str.split("").filter(function(e){ return e.toUpperCase() == e }).join("");
}
