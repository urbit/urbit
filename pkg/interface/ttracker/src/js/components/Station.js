import React, { Component } from 'react';
import { faCaretDown } from "@fortawesome/free-solid-svg-icons";
import { faCaretUp } from "@fortawesome/free-solid-svg-icons";
import {faWheelchair } from "@fortawesome/free-solid-svg-icons";
import {faBicycle } from "@fortawesome/free-solid-svg-icons";
import {faParking } from "@fortawesome/free-solid-svg-icons";
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import L from 'leaflet'

import _ from 'lodash';
import { MoreInfo } from './MoreInfo';




export class Station extends Component {
    constructor(props) {
        super(props)
        var url = window.location.href
        let splitUrl = url.split('/');
        let stationName = splitUrl[splitUrl.length - 1];
        let stations = this.getStation(this.props.stations,stationName);
       let station = stations[0];
        this.state = {
            station: station
        }
        this.parseFacilities = this.parseFacilities.bind(this);
        this.getElevators = this.getElevators.bind(this);
        console.log(this.props.facilities)
    }



    

    componentDidMount () {
       

        
        api.action("ttracker","ttracker-action",{
            facilities: {
                station:this.state.station.relationships.parent_station.data.id
            }
        })
        let latlng = L.latLng(this.state.station.attributes.latitude, this.state.station.attributes.longitude);
        let myMap = L.map('mapid', {
            
            center: latlng,
            zoom: 16,
            layers: [
              L.tileLayer('http://{s}.tile.osm.org/{z}/{x}/{y}.png', {
                attribution: '&copy; <a href="http://osm.org/copyright">OpenStreetMap</a> contributors'
              }),
            ]
          });
          L.marker(latlng).addTo(myMap);
         
     }



    render() {
        let bgGrey = {
            background: '#CCCCCC'
        };
        
        let mapStyle =  {
            width: '100%', height: '400px'
        };
        <h2>
                    Line:
                </h2>
      
               
        return(
            <div>
                <h1 className='mt2'>
                {this.state.station.attributes.name}
                </h1>
                <div style={mapStyle} className='mt2'  id="mapid">
                </div>

                <h1 className='mt2'>Elevators</h1>
              
                   

                      
                        {this.getElevators(this.props.facilities)}
                    
                    
                
                

            </div>
        );
            
    
           
            
    }
    getStation(stations,stationName) {
        var station = _.map(stations.data, function(o) {
            if(o.relationships.parent_station === null) undefined
            if(o.relationships.parent_station.data.id  === stationName) return o;
            
        });
        var without =  _.without(station,undefined)
        console.log(without);
        return without;
    }

    parseFacilities(facilities) {
        console.log(facilities)
        getElevators(facilities);
        getEscalators(facilities);
       
    }

    getElevators(facilities) {
        if(!facilities) {
            return null
        }
        else {
            var elev = _.map(facilities.data, function(o) {
                if(o.attributes.type === 'ELEVATOR') return o;                
            });
            var without =  _.without(elev,undefined)
            console.log(without);
            const listItems = without.map((number) =>
                <li>{number.attributes.long_name}</li>)
            return(
            
            <ul>{listItems}</ul>
            )

                
        }
        
    }

    




    
    

}