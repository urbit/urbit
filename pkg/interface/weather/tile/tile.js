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
      latlng: ''
    };
  }

  locationSubmit() {
    console.log('location submit');
    navigator.geolocation.getCurrentPosition((res) => {
      console.log(res);
      let latlng = `${res.coords.latitude},${res.coords.longitude}`;
      this.setState({
        latlng
      }, (err) => {
        console.log(err);
      }, { maximumAge: Infinity, timeout: 10000 });
      api.action('weather', 'json', latlng);
    });
  }

  renderWrapper(child) {
    return (
      <div className="pa2 relative" style={{
        width: 234,
        height: 234,
        background: '#1a1a1a'
      }}>
        {child}
      </div>
    );
  }

  renderNoData() {
    return this.renderWrapper((
      <div onClick={this.locationSubmit.bind(this)}>
          <p className="gray label-regular b absolute"
            style={{left: 8, top: 4}}>
            Weather
          </p>
        <p className="absolute w-100 flex-col body-regular white" style={{verticalAlign: "bottom", bottom: 8, left: 8}}>Set location</p>
      </div>
    ));
  }

  renderWithData(data) {
    let c = data.currently;
    let d = data.daily.data[0];

    let da = moment.unix(d.sunsetTime).format('h:mm a') || '';

    return this.renderWrapper((
      <div>
          <p className="gray" style={{
            fontWeight: 'bold',
            fontSize: 14,
            lineHeight: '24px'
          }}>Weather</p>
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
