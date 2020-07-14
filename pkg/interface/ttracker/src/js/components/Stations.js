import React, { Component } from 'react';
import _ from 'lodash';
import { ButtonList } from './ButtonList';


export class Stations extends Component {
    constructor(props) {
      super(props);
      this.state = {
        chunkedRouteNames: this.chunkAray(this.props.routes.data,3),
            selectedLine: 'Red',
            stationsByLine: this.getStationsByLineType(this.props.stops.data,'Red'),
            chunked: this.chunkAray(this.getStationsByLineType(this.props.stops.data,'Red'),3)
    }
      
      
       
      this.switchSelectedLine = this.switchSelectedLine.bind(this);
      this.getStationsByLineType = this.getStationsByLineType.bind(this)
      this.chunkAray = this.chunkAray.bind(this);

    }

   

   


    render() {
      return(
            <div  className="w-100 h-25">
                <h1>
                    Red Line
                </h1>
                <div className="flex flex-wrap mt2 "> 
                    <ButtonList stationsList={this.state.chunkedRouteNames[0]}
                                buttonType='Round'
                                elementType='Button'
                                buttonClicked={this.switchSelectedLine}
                                selected= {this.state.selectedLine}
                    >
                    </ButtonList>
                    <ButtonList stationsList={this.state.chunkedRouteNames[1]}
                                buttonType='Round'
                                elementType='Button'
                                buttonClicked={this.switchSelectedLine}
                                selected= {this.state.selectedLine}

                    >
                    </ButtonList>
                    <ButtonList stationsList={this.state.chunkedRouteNames[2]}
                                buttonType='Round'
                                elementType='Button'
                                buttonClicked={this.switchSelectedLine}
                                selected= {this.state.selectedLine}

                    >
                    </ButtonList>
                    </div>
                    <div className="flex flex-wrap mt2 "> 
                   
                    {this.state.chunked.map(stationList => (
                    
                <ButtonList stationsList={stationList}
                                buttonType='Square'
                                elementType='Link'
                                buttonClicked={this.switchSelectedLine}
                                selected= {this.state.selectedLine}

                    >
                    </ButtonList>
                
                ))}
                </div> 
                    
                    
                

            </div>
        );
    }

   

    

    switchSelectedLine(lineName) {
          this.setState({
                chunkedRouteNames: this.chunkAray(this.props.routes.data,3),
                selectedLine: lineName,
                stationsByLine: this.getStationsByLineType(this.props.stops.data,lineName),
                chunked: this.chunkAray(this.getStationsByLineType(this.props.stops.data,lineName),3)
            
          });
    }

    

    getStationsByLineType(stations,lineType) {
        let routeMap = {
            'Red': "Red Line",
            'Mattapan': 'Mattapan Trolley',
            'Orange': 'Orange Line',
            'Blue': 'Blue Line',
            'Green': 'Green Line'
            }
        console.log(stations)
        var alerts = _.map(stations, function(o) {
            let splitDesc = o.attributes.description.split('-')
            if(splitDesc.length > 2) {
                if(splitDesc[2].includes('(B)') && lineType === 'Green-B') {
                    return o;
                }
                if(splitDesc[2].includes('(C)') && lineType === 'Green-C') {
                    return o;
                }
                else if(splitDesc[2].includes('(D)') && lineType === 'Green-D') {
                            return o;
                        }
                else if(splitDesc[2].includes('(E)') && lineType === 'Green-E') {
                            return o;
                        }
                else if(splitDesc[1] === 'Green Line') {
                            return o
                }
            }
            if (splitDesc[1].trim() === routeMap[lineType]) return o;
            
        });
        var without =  _.without(alerts,undefined)
        console.log(without);
        return _.uniqBy(without, function (e) {
            return e.attributes.name;
          });
            
    }

    chunkAray(array,size) {
        const chunkedArray = []
         for (var i = 0; i < array.length; i += size) {
            chunkedArray.push(array.slice(i, i + size))
        }
        return chunkedArray
    }

   
}
