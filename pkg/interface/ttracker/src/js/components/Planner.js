import React, { Component } from 'react';
import { BrowserRouter, Route, Switch } from "react-router-dom";
import Select from 'react-select';
import queryString from 'query-string';
import moment from 'moment'


import _ from 'lodash';

export class Planner extends Component {
    constructor(props) {
      super(props);
      let currTime = moment(new Date(),"YYYY/MM/DD");
      this.state = {
        fromStation: {},
        toStation: {},
        hours: '',
        min: '',
        AMorPM: '',
        day: currTime.format('MM'),
        month: currTime.format('DD'),
        year: currTime.format('YYYY'),
        shouldDisplayRoute: false
               

      }

      //handlers
      this.handleToChange = this.handleToChange.bind(this);
      this.handleFromChange = this.handleFromChange.bind(this);

      this.handleYearChange = this.handleYearChange.bind(this);
      this.handleMonthChange = this.handleMonthChange.bind(this);

      this.handleDayChange = this.handleDayChange.bind(this);
      this.handleHrChange = this.handleHrChange.bind(this);
      this.handleMinChange = this.handleMinChange.bind(this);
      this.handleAMChange = this.handleAMChange.bind(this);
      //ranges

      this.getHourRange = this.getHourRange.bind(this);
      this.getMinuteRange = this.getMinuteRange.bind(this);
      this.getAMPMRange = this.getAMPMRange.bind(this);
     
     
     
     
      this.getDayRange = this.getDayRange.bind(this);
      this.getHourRange = this.getHourRange.bind(this);
      this.getMinuteRange = this.getMinuteRange.bind(this);
      this.handleLeaveNow = this.handleLeaveNow.bind(this)
      this.getTripPlan = this.getTripPlan.bind(this)




    }

    handleFromChange(selectedOption) {
      
      const value = selectedOption;
      this.setState({
        fromStation: selectedOption.value
      });
    }


    handleToChange(selectedOption) {
      
      const value = selectedOption;
      this.setState({
        toStation: selectedOption.value
      });
    }

    handleHrChange(selectedOption) {
      this.setState({
        hours: selectedOption.value
      });
    }

    handleMinChange(selectedOption) {
      
      this.setState({
        min: selectedOption.value
      });
    }

    handleAMChange(selectedOption) {
      
      this.setState({
        AMorPM: selectedOption.value
      });
    }


    handleMonthChange(selectedOption) {
      
      this.setState({
        month: selectedOption.value
      });
    }


    handleDayChange(selectedOption) {
      
      this.setState({
        day: selectedOption.value
      });
    }


    handleYearChange(selectedOption) {
      
      this.setState({
        year: selectedOption.value
      });
    }

    getTripPlan = async () => {
      const response = await fetch('https://www.mbta.com/trip-planner?plan%5Bfrom%5D=South+Station&plan%5Bfrom_latitude%5D=42.352271&plan%5Bfrom_longitude%5D=-71.055242&plan%5Bto%5D=North+Station&plan%5Bto_latitude%5D=42.365577&plan%5Bto_longitude%5D=-71.06129&plan%5Btime%5D=depart&plan%5Bdate_time%5D%5Bhour%5D=12&plan%5Bdate_time%5D%5Bminute%5D=00&plan%5Bdate_time%5D%5Bam_pm%5D=AM&plan%5Bdate_time%5D%5Bmonth%5D=7&plan%5Bdate_time%5D%5Bday%5D=12&plan%5Bdate_time%5D%5Byear%5D=2020&plan%5Bmodes%5D%5Bsubway%5D=true&plan%5Bmodes%5D%5Bcommuter_rail%5D=true&plan%5Bmodes%5D%5Bbus%5D=true&plan%5Boptimize_for%5D=best_route');
      console.log(response);
    }

    

    render() {
     
      
     let idClasses =
      "f7 ba b--gray3 b--gray2-d bg-gray0-d white-d pa3 db w-50 " +
      "focus-b--black focus-b--white-d "; 

      let contentBox = {
        boxSizing: 'border-box'
      }
    let options = this.props.stops.data.map((station) => {
      return { value: station, label: station.attributes.name }
    })
          return (
          
         

         
            <div
              className={
                "h-100 w-100 mw6 pa3 pt4 overflow-x-hidden " +
                "bg-gray0-d white-d flex flex-column no-scrollbar"
              }>
              <h2 className="mb1 f8">Trip Planner</h2>
              <div className="w-100">
                <p className="f8 mt3 lh-copy w-80 b">From
                </p>
                <Select 
                name="fromStation"
                placeholder='Where are you now?'
                className='w-50'
                options={options}
                onChange={this.handleFromChange}

                />
                <p className="f8 mt3 lh-copy w-80 b">
                  To
                </p>
                <Select 
                name="toStation"
                placeholder="Where are you headed?"
                className='w-50'
                options={options}
                onChange={this.handleToChange}

                 />
              <p className="f8 mt3 lh-copy b">When</p>
              <span className="flex">
                
                          <Select 
                            name="year"
                            className='w-25 mr2'
                            placeholder='Year'
                            options={this.getYearRange()}
                            onChange={this.handleYearChange}
                            />
                            <Select 
                            name="month"
                            className='w-25 mr2'
                            placeholder="Month"
                            options={this.getMonthsRange()}
                            onChange={this.handleMonthChange}

                            />
                            <Select 
                            name="day"
                            className='w-25 mr2'
                            placeholder="Day"
                            options={this.getDayRange(this.state.year,this.state.month)}
                            onChange={this.handleDayChange}

                            />
                 </span>
                 <span className="flex mt2">

              
                        <Select 
                          name="hour"
                          className='w-25 mr2'
                          placeholder='Hour'
                          options={this.getHourRange()}
                          onChange={this.handleHrChange}
                          />
                          <Select 
                          name="min"
                          className='w-25 mr2'
                          placeholder="Min"
                          options={this.getMinuteRange()}
                          onChange={this.handleMinChange}

                          />
                          <Select 
                          name="AMPM"
                          className='w-25 mr2'
                          placeholder="AM/PM"
                          options={this.getAMPMRange()}
                          onChange={this.handleAMChange}

                          />
                 </span>
              
               <span className="flex">
               <button onClick={this.getTripPlan} className="pointer db f9 mt7 gray2 ba bg-gray0-d pa2 pv3 ph4 b--gray3 w-25 mr2">
                Leave Now
                </button>
               </span>
                </div>
            </div>
      
            
           
          );
    }

    getHourRange() {
      let hours = [];
      for(var i = 1; i <= 12; i++) {
        hours.push ({ value: i.toString(), label: i.toString() })

      }

      return hours;

    }

    //minutes in sets of 5
    getMinuteRange() {
      let minutes = [];
      for(var i = 0; i <= 60; i+=5) {
          if(i == 0) {
            minutes.push ({ value: '00', label: '00' })
          }else if (i == 5) {
            minutes.push ({ value: '05', label: '05' })

        }
        else {
          minutes.push ({ value: i.toString(), label: i.toString() })
        }
      }
      return minutes;
    }

    getAMPMRange() {
      let ampm = [];

    ampm.push({ value: 'AM', label: 'AM' })
    ampm.push({value:'PM' ,label: 'PM'})
    return ampm;
    }

    handleLeaveNow() {
      let now = moment(new Date(),'ddd DD-MMM-YYYY, hh:mm A');
      let urlStr = '/plannerResult?' + 'toStation=' + this.state.toStation.attributes.name.trim().replace(' ','+');
      urlStr += '&fromStation=' + this.state.fromStation.attributes.name.trim().replace(' ','+');
       urlStr += '&hours=' +  now.hour() % 12;
       //Roound up to nearest minute
       urlStr += '&minutes=' + Math.ceil(now.minutes() / 5) * 5;
       let amPM = now.format('hh:mm A').split(' ');
       urlStr += '&am=' + amPM[1];
       urlStr += '&from_latitude=' + this.state.fromStation.attributes.latitude;
       urlStr += '&from_longitude=' + this.state.fromStation.attributes.longitude;
       urlStr += '&to_latitude=' + this.state.toStation.attributes.latitude;
       urlStr += '&to_longitude=' + this.state.toStation.attributes.longitude;
       urlStr += '&year=' + now.year();
       urlStr += '&month=' + now.month() + 1;
       urlStr += '&day=' + now.date();
       console.log(urlStr);   

      }
    

  

    getDayRange(year,month) {
       let numDaysInMonth = moment(year + '-' + (month+1), "YYYY-M").daysInMonth();
       let nums = [];
       for(var i = 1; i <= numDaysInMonth; i++) {
         nums.push({ value: i, label: i });
       }
       return nums
       
    }
    

    getMonthsRange() {
      return _.map(moment.months(), function(month,index) {
        return { value: index, label: month }
      });
    }

    getYearRange() {
      let years = [];
      let year = moment(new Date()).format('YYYY');
      let nextYear = moment(new Date()).add(1, 'years').format('YYYY');
      years.push({ value: year, label: year })
      years.push({value: nextYear ,label: nextYear})
      return years;
    }

  
}