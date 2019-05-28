import React, { Component } from 'react';
import classnames from 'classnames';
import moment from 'moment';

class IconWithData extends Component {
  render() {
    const { props } = this;

    return (
      <div className='mt2'>
        <img 
          src={'/~weather/img/' + props.icon + '.png'} 
          width={20} 
          height={20}
          className="dib mr2" />
        <p className="label-small dib white">{props.text}</p>
      </div>
    ); 
  }
}

export default class WeatherTile extends Component {

  constructor(props) {
    super(props);

    let ship = window.ship;
    let api = window.api;

    this.state = {
      location: '',
      latlng: ''
    };
  }

  locationChange(e) {
    this.setState({
      location: e.target.value
    });
  }

  firstSubmit() {
    if (!this.state.location) {
      return;
    }

    this.askForLatLong(this.state.location)
  }

  locationSubmit() {
    if (!this.state.location) {
      return;
    }

    api.action('weather', 'json', this.state.latlng);
  }

  askForLatLong(place) {
    let url = 'https://maps.googleapis.com/maps/api/geocode/json?address=';
    let key = '&key=AIzaSyDawAoOCGSB6nzge6J9yPnnZH2VUFuG24E';
    fetch(url + encodeURI(place) + key)
      .then((obj) => {
        return obj.json();
      }).then((json) => {
        console.log(json);
        if (json && json.results && json.results.length > 0) {
          this.setState({
            latlng: 
              json.results[0].geometry.location.lat
              + ',' 
              + json.results[0].geometry.location.lng
          });
        }
      });
  }

  renderWrapper(child) {
    return (
      <div className="pa2 bg-dark-gray" style={{ width: 234, height: 234 }}>
        {child}
      </div>
    );
  }

  renderNoData() {
    return this.renderWrapper((
      <div>
        <p className="white sans-serif">Weather</p>
        <input type="text" onChange={this.locationChange.bind(this)} />
        {this.state.latlng}
        <button onClick={this.firstSubmit.bind(this)}>Submit</button>
        <button onClick={this.locationSubmit.bind(this)}>Go</button>
      </div>
    ));
  }

  renderWithData(data) {
    let c = data.currently;
    let d = data.daily.data[0];

    let da = moment.unix(d.sunsetTime).format('h:mm a') || '';

    return this.renderWrapper((
      <div>
        <p className="white">Weather</p>
        <div className="w-100 mb2 mt2">
          <img 
            src={'/~weather/img/' + c.icon + '.png'} 
            width={64} 
            height={64}
            className="dib" />
          <h2 
            className="dib ml2 white"
            style={{
              fontSize: 72,
              lineHeight: '64px',
              fontWeight: 400
            }}>
            {Math.round(c.temperature)}°</h2>
        </div>
        <div className="w-100 cf">
          <div className="fl w-50">
            <IconWithData 
              icon='winddirection'
              text={c.windBearing + '°'} />
            <IconWithData 
              icon='chancerain'
              text={c.precipProbability + '%'} />
            <IconWithData 
              icon='windspeed'
              text={Math.round(c.windSpeed) + ' mph'} />
          </div>
          <div className="fr w-50">
            <IconWithData 
              icon='sunset'
              text={da} />
            <IconWithData 
              icon='low'
              text={Math.round(d.temperatureLow) + '°'} />
            <IconWithData 
              icon='high'
              text={Math.round(d.temperatureHigh) + '°'} />
          </div>
        </div>
      </div>
    ));
  }

  render() {
    let data = !!this.props.data ? this.props.data : {};

    if ('currently' in data && 'daily' in data) {
      return this.renderWithData(data);
    }

    return this.renderNoData();
  }

}

window.weatherTile = WeatherTile;
