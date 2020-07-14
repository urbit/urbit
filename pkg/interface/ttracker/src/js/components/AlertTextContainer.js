import React, { Component } from 'react';
import { BrowserRouter, Route, Switch } from "react-router-dom";
import _ from 'lodash';
import { faCaretDown } from "@fortawesome/free-solid-svg-icons";
import { faCaretUp } from "@fortawesome/free-solid-svg-icons";
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import { MoreInfo } from './MoreInfo';


export class AlertTextContainer extends Component {
    constructor(props) {
      super(props);
      this.state = { dropdownOpen: [] };
      this.toggleDropdown = this.toggleDropdown.bind(this);


    }

    componentDidMount() {
      let stationsWithDropDown = this.props.alerts.map( () => {
        return false;
      });
      this.setState({
        dropdownOpen: stationsWithDropDown
    });
    }

    render() {
      

      if(this.props.alerts.length === 0)  return(null);
      let alerts = this.props.alerts.map((alert, index) => {
        return (
          <div>

         
          <li className="ph3 pv3 bb b--light-silver">
                <div>
                    <ul key={index} className="list pl0 mb2 ml4">
                      <li className="dib mr4">
                          <b>
                          {alert.attributes.effect}
                          </b>
                      </li>
                      <div className="dib bg-moon-gray">
                          <li >
                            {alert.attributes.lifecycle}
                          </li>
                      </div>
                    </ul>
                </div>
                <div className="ml4 flex mb2">
                    <div className="w-90">
                      {alert.attributes.header}
                    </div>
                    <div className="w-10" onClick={ () => this.toggleDropdown(index) }>
                      {this.state.dropdownOpen[index]
                      ? <FontAwesomeIcon className="flex justify-center content-center ml4" icon={faCaretUp} size="1x"/>
                      : <FontAwesomeIcon className="flex justify-center content-center ml4" icon={faCaretDown} size="1x" />

                      }
                      
                    </div>
                </div>
                <MoreInfo isOpen={this.state.dropdownOpen[index]} 
                          text={alert.attributes.description}>

                </MoreInfo>
                
                    
              </li>
              </div>
        )
      })
        return(
          <article class="pa3 pa5-ns">
            <ul className="list pl0 ml0 center mw6 ba b--light-silver br2">
              
              {alerts}
            </ul>
        </article>
            
        );
    }


    toggleDropdown(index) {
      console.log(index)
      let openStates = [...this.state.dropdownOpen];    
      openStates[index] = !openStates[index];                
      this.setState({ dropdownOpen: openStates });
      
  }
    

  }
