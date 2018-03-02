HTMLWidgets.widget({

  name: 'function-plot',

  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance

    return {

      renderValue: function(x) {

        window[el.id] = functionPlot({
          target: '#' + el.id,
          title: x.title,
          width: x.width,
          height: x.height,
          disableZoom: x.disableZoom,
          xAxis: x.xAxis,
          yAxis: x.yAxis,
          annotations: x.annotations,
          grid: x.grid,
          data: x.data
        });

        window[el.id];

      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});
