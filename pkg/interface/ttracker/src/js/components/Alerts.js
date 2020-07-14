import React, { Component } from 'react';
import { BrowserRouter, Route, Switch } from "react-router-dom";
import _ from 'lodash';
import {StationAlerts} from './StationAlerts'

export class Alerts extends Component {
    constructor(props) {
      super(props);
    }


     


    render() {
        return(
        <div className="center mw6 h-100">
            <div className="h-100 pt0 pt8-m pt8-l pt8-xl no-scrollbar">
                <div className="flex justify-between" style={{ marginBottom: 32 }}>
                    <div className="flex-col">
                        <div className="mb1 f12 center "><b> Alerts</b></div>
                    </div>
                </div>
                <StationAlerts alerts={this.props.alerts} routes={this.props.routes}>
                    
                </StationAlerts>

                
            </div>
        
        </div>
        );
    }
}