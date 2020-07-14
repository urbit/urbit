import React, { Component } from 'react';
import { BrowserRouter, Route, Switch } from "react-router-dom";
import _ from 'lodash';
import { faRedo } from '@fortawesome/free-solid-svg-icons';
import { ButtonList } from './ButtonList';
import {AlertTextContainer} from './AlertTextContainer'

import Select from 'react-select'




export class Schedule extends Component {

    constructor(props) {
      super(props);
      this.state = {
        selectedLine: 'Red Line',
        selectedStation: '',
        selectedDirection: '',
        selectedInfoType: 'Schedule',
        lines: [['Red Line', 'Mattapan Trolley', 'Orange Line'], ['Blue Line', 'Green Line', 'B'], ['C','D','E']],
        selectedDate: '',
        alerts: {
          redStations: [
            {
              alertType: 'Station Issue',
              timeline: 'Ongoing',
              previewMessage: 'Beginning Wednesday, April 15, the main entrance and exit to the Alewife garage will be closed for two weeks for construction. Please follow posted signs to access the garage. Customers will only be able to use PayByPhone to pay for parking.',
              showMore: 'To access the garage drivers will use the spiral exit ramp which will be reconfigured for two-way traffic. Customers will enter the garage on the 4th level and exit from the 3rd level. \r\n\r\nPayByPhone must be used to pay for parking during this closure. Cash will not be accepted. Payment can be made via the PayByPhone app, website or by calling 1-866-234-7275. The location code is for the garage is 4395'
            },
            {
              alertType:'Station Issue',
              timeline:'Ongoing',
              previewMessage:"Beginning Wednesday, April 15, the main entrance and exit to the Alewife garage will be closed for two weeks for construction. Please follow posted signs to access the garage. Customers will only be able to use PayByPhone to pay for parking.",
              showMore:'To access the garage drivers will use the spiral exit ramp which will be reconfigured for two-way traffic. Customers will enter the garage on the 4th level and exit from the 3rd level. \r\n\r\nPayByPhone must be used to pay for parking during this closure. Cash will not be accepted. Payment can be made via the PayByPhone app, website or by calling 1-866-234-7275. The location code is for the garage is 4395'
            }
        ],
        orangeLine: [
          {
              alertType:'Service Change',
              timeline:'Ongoing',
              previewMessage:"North Station will be bypassed southbound on weekends from June 6 - 7 to July 11 - 12 beginning Friday evenings at 8:45 PM. Customers can exit at Haymarket and transfer to the Green Line or Orange Line to return to North Station",
              showMore:'Regular service will operate on July 4th Weekend. North Station northbound is scheduled to be bypassed for five weekends beginning July 17. This platform closure is to allow for maintenance work at the station. '
          }
        ]
  
        }

          
      }

      this.switchSelectedLine = this.switchSelectedLine.bind(this);
      this.switchSelectedInfoType = this.switchSelectedInfoType.bind(this);
      this.handleStationChange = this.handleStationChange.bind(this);
      this.handleDirectionChange = this.handleDirectionChange.bind(this);
      this.handleDateChange = this.handleDateChange.bind(this);

    }

    

    


    render() {
        console.log(this.state);
        console.log(this.allFieldsSet())
        let baseButton = 'w-30 pa3 mr2 tc f6 no-underline br-pill ba bw1 ph3 pv2 mb2 ' 
        let buttonRegular  = baseButton + 'black'
        let buttonSelected = baseButton + 'white bg-black'
        let buttonSquareBase = 'w-30 pa3 mr2 tc f6 no-underline ba bw1 ph3 pv2 mb2 '
        let buttonSquareRegular = buttonSquareBase + 'black'
        let buttonSquareSelected = buttonSquareBase + 'white bg-black'

      

        return(
            <div>
                <hr className="mt3"></hr>
                <h2>
                    Line:
                </h2>

                <div className="flex flex-wrap mt2 ">    

                    <ButtonList stationsList={this.state.lines[0]}
                                buttonType='Round'
                                buttonClicked={this.switchSelectedLine}
                                selected= {this.state.selectedLine}
                                elementType= 'Button'>
                                
                    </ButtonList>
                    
                    
                    
                                    </div>
                <div className="flex flex-wrap">    
               
                <ButtonList stationsList={this.state.lines[1]}
                                buttonType='Round'
                                buttonClicked={this.switchSelectedLine}
                                selected= {this.state.selectedSelectedLine}
                                elementType= 'Button'>
                </ButtonList>
                    

                 </div>
                <div className="flex flex-wrap">    
                <ButtonList stationsList={this.state.lines[2]}
                                buttonType='Round'
                                buttonClicked={this.switchSelectedLine}
                                selected= {this.state.selectedLine}
                                elementType= 'Button'>
                </ButtonList>
                
                
                
                    
                </div>
                <hr>
                </hr>
                <div className="flex flex-wrap mt2"> 
                <ButtonList stationsList={['Schedule','Alerts']}
                                buttonType='Square'
                                buttonClicked={this.switchSelectedInfoType}
                                selected= {this.state.selectedInfoType}
                                elementType= 'Button'>
                </ButtonList>
                </div>
                <hr></hr>
                {this.state.selectedInfoType === 'Schedule' && this.fields()}
                {this.state.selectedInfoType === 'Alerts' && this.alerts()}

                
                <hr></hr>
                {this.allFieldsSet() && this.state.selectedInfoType === 'Schedule' &&
                <h2>
                Schedule
                </h2>}
                {this.allFieldsSet() && 
                  
                  this.schedule()
                
                }

                


                

    <h2 className="mt2">Maps</h2>
    <h3 onClick={()=> window.open("https://cdn.mbta.com/sites/default/files/route_pdfs/2020-spring/rtRapid.pdf", "_blank")} className=" ml2 underline">Subway</h3> 
    <h3 onClick={()=> window.open("https://cdn.mbta.com/sites/default/files/route_pdfs/2020-spring/rtRapid.pdf", "_blank")} className="ml2 underline">Subway and Timetable</h3>


             </div>

            
        )
            
    }

    alerts() {
      if(this.state.selectedLine === 'Red Line') {
        return (
          <AlertTextContainer stations={this.state.alerts.redStations}>
            </AlertTextContainer>
        )
      } else {
        return(
        <AlertTextContainer stations={this.state.alerts.orangeLine}>
            </AlertTextContainer>
        );
      }
    }

    switchSelectedLine(lineName) {
      console.log(lineName);
        this.setState({
            selectedLine: lineName,
            selectedStation: null
        });
      }

      switchSelectedInfoType(infoType) {
          console.log(infoType);
        this.setState({
            selectedInfoType: infoType,
            selectedDate: null
        });
      }


      fields() {
        let idClasses =
      "f7 ba b--gray3 b--gray2-d bg-gray0-d white-d db w-30 " +
      "focus-b--black focus-b--white-d"; 

      let contentBox = {
        boxSizing: 'border-box'
      }

      // let idErrElem = (<span />);
    // if (state.idError) {
    //   idErrElem = (
    //     <span className="f9 inter red2 db pt2">
    //       Collection must have a valid name.
    //     </span>
    //   );
    // }

    let redStations = ['Alefwife', 'Andrew', 'Ashmont',
    'Braintree', 'Broadway', 'Central','Charles/MGH', 'Davis', 'Downtown Crossing',
    'Fields Corner', 'Harvard', 'JFK/UMASS','Kendall/MIT', 'North Quincy', 'Park Street',
    'Porter', 'Quincy Adams', 'Quincy Center','Savin Hill', 'Shawmut', 'South Station']
    
    let orangeStations = ['Assembly', 'Back Bay', 'Chinatown', 'Community College', 'Downtown Crossing', 'Forest Hills',
    'Green Street', 'Haymarket', 'Jackson Square', 'Malden Center', 'Massachusets Avenue', 'North Station',
    'Oak Grove', 'Roxbury Crossing', 'Ruggles','State', 'Stony Brook', 'Sullivan Square', 'Tufts Medical Center', 'Wellington']

  

    let redOptions = redStations.map((station) => {
      return { value: station, label: station }
    })
        
    let orangeOptions = orangeStations.map((station) => {
      return { value: station, label: station }
    })

    let directionOptions = ['Northbound', 'Southbound'].map((station) => {
      return { value: station, label: station }
    });


        return (
          
          
              <div className="w-100 flex pr2 pt2 pb2">
                {/* <p className="f8 mt3 lh-copy w-80 b">From:</p>
                <input
                  className={idClasses}
                  placeholder="Where are you now?"
                  type="text"
                  
                /> */}
              

                <Select 
                placeholder='Station Name'
                value={this.state.selectedStation}
                className='w-30 mr2'
                options={this.state.selectedLine === 'Red Line' ? redOptions : orangeOptions }
                onChange={this.handleStationChange}
                theme={(theme) => ({
                  ...theme,
                  borderRadius: 0,
                  colors: {
                  ...theme.colors,
                    text: 'white',
                    primary: 'black',
                  },
                })} />
              
                <Select 
                placeholder="Direction"
                onChange={this.handleDirectionChange}
                className='w-30 mr2'
                options={directionOptions}
                theme={(theme) => ({
                  ...theme,
                  borderRadius: 0,
                  colors: {
                  ...theme.colors,
                    text: 'white',
                    primary: 'black',
                  },
                })} />
                 <input onChange={this.handleDateChange} type="text" 

              style= {
                contentBox
              }
              type="date"
                      value={this.selectedDate} min={new Date()} max="2020-06-20" defaultValue={new Date} className={idClasses}>
               </input>
               
              
                
               </div>
              
        );
      }

      handleDirectionChange(selectedOption) {
        this.setState({ selectedDirection: selectedOption })
      }

      handleDateChange(selectedOption) {
        this.setState({ selectedDate: selectedOption.target.value })

      }

      handleStationChange(selectedOption) {
        this.setState({ selectedStation: selectedOption })

      }

      allFieldsSet() {
        return this.state.selectedDate !== '' && this.state.selectedDirection !== '' && this.state.selectedStation !== ''  && this.state.selectedDate !== null && this.state.selectedStation !== null ;
      }

      schedule() {
        if(this.state.selectedLine === 'Red Line') {
          return (
          
                
            <div class="pa4 ml2">
            <div class="overflow-auto">
              <table class="f6 w-100 mw8" cellspacing="0">
                <thead>
                  <tr>
                    <th class="fw6 bb b--black-20 tl pb3 pr3 bg-white">Scheduled</th>
                    <th class="fw6 bb b--black-20 tl pb3 pr3 bg-white">Northbound to</th>
          
                  </tr>
                </thead>
                <tbody class="lh-copy">
                  <tr>
                    <td class="pv3 pr3 bb b--black-20">01:50P</td>
                    <td class="pv3 pr3 bb b--black-20">Alefwife</td>
          
                  </tr>
                  <tr>
                    <td class="pv3 pr3 bb b--black-20">02:03P</td>
                    <td class="pv3 pr3 bb b--black-20">Alefwife</td>
          
                  </tr>
                  <tr>
                    <td class="pv3 pr3 bb b--black-20">02:17P</td>
                    <td class="pv3 pr3 bb b--black-20">Alefwife</td>
                  </tr>
                  <tr>
                    <td class="pv3 pr3 bb b--black-20">02:30P</td>
                    <td class="pv3 pr3 bb b--black-20">Alefwife</td>
                  </tr>
                  <tr>
                    <td class="pv3 pr3 bb b--black-20">02:44P</td>
                    <td class="pv3 pr3 bb b--black-20">Alefwife</td>
                  </tr>
                </tbody>
              </table>
            </div>
          </div>
                  );
        }
        else {
          return (
          
                
            <div class="pa4 ml2">
            <div class="overflow-auto">
              <table class="f6 w-100 mw8" cellspacing="0">
                <thead>
                  <tr>
                    <th class="fw6 bb b--black-20 tl pb3 pr3 bg-white">Scheduled</th>
                    <th class="fw6 bb b--black-20 tl pb3 pr3 bg-white">Northbound to</th>
          
                  </tr>
                </thead>
                <tbody class="lh-copy">
                  <tr>
                    <td class="pv3 pr3 bb b--black-20">011:50A</td>
                    <td class="pv3 pr3 bb b--black-20">Oak Grove</td>
          
                  </tr>
                  <tr>
                    <td class="pv3 pr3 bb b--black-20">12:00P</td>
                    <td class="pv3 pr3 bb b--black-20">Oak Grove</td>
          
                  </tr>
                  <tr>
                    <td class="pv3 pr3 bb b--black-20">12:10P</td>
                    <td class="pv3 pr3 bb b--black-20">Oak Grove</td>
                  </tr>
                  <tr>
                    <td class="pv3 pr3 bb b--black-20">12:10P</td>
                    <td class="pv3 pr3 bb b--black-20">Oak Grove</td>
                  </tr>
                  <tr>
                    <td class="pv3 pr3 bb b--black-20">12:20P</td>
                    <td class="pv3 pr3 bb b--black-20">Oak Grove</td>
                  </tr>
                </tbody>
              </table>
            </div>
          </div>
                  );
        }
        
      }


     

}