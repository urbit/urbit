import React, { Component } from 'react';
import _ from 'lodash';
import {Tile} from "./Tile"
import { faExclamationTriangle, faSubway, faMap, faSearchLocation } from "@fortawesome/free-solid-svg-icons";
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'


export class Home extends Component {
    constructor(props) {
      super(props);
    }

    render() {
        return(
            <div>
                
          <Tile linkPath="/~ttracker/stations" pageName="Stations" tileIcon={faSubway}>
              
          </Tile>
          <Tile linkPath="/~ttracker/planner" pageName="Trip Planner"  tileIcon={faSearchLocation}>
              
          </Tile>
          <div className="fl ma2 bg-white bg-gray0-d overflow-hidden"
           style={{ height: '126px', width: '126px' }}>
               <div className="w-100 h-100 relative bg-white bg-gray0-d ba b--black b--gray1-d">
               <a className="w-100 h-100 db pa2 no-underline" target="_blank" rel="noopener noreferrer" href="https://cdn.mbta.com/sites/default/files/route_pdfs/2020-spring/rtRapid.pdf">
                       <p className="black white-d absolute f9" style={{left: '8px', top: '8px'}}>Maps and Schedule </p>
                       <div className="absolute invert-d" style={{top: '45px', left: '45px'}}>
                       <FontAwesomeIcon icon={faMap} size="3x" />
                       </div>
                       
                    </a>
                    
                </div>
                
      </div>
          <Tile linkPath="/~ttracker/alerts" pageName="Alerts" tileIcon={faExclamationTriangle}>
              
          </Tile>
          
            </div>
               
        );
        
    }
}