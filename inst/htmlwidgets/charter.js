HTMLWidgets.widget({

  name: 'charter',

  type: 'output',

  factory: function(el, width, height) {

    var ctx,
        chart;

    return {

      renderValue: function(x) {
        var ctx = document.getElementById(el.id).getContext('2d');
        var chart = new Chart(ctx, x.opts);

      },

      get_chart: function(){
        return chart;
      },

      resize: function(width, height) {

        if(chart)
          chart.resize();

      }

    };
  }
});