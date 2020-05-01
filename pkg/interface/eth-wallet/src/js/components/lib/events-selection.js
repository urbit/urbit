import React, { Component } from 'react';
import { Checkbox } from './checkbox';
import {getEventStructureByName} from '../../lib/events';

export class EventsSelection extends Component {
  constructor(props) {
    super(props);
    this.state = {
      selectedEvents: [],
      listenToAllEvents: true,
    }
  }

  render() {
    const { abi } = this.props;
    const { selectedEvents, listenToAllEvents } = this.state;
    if (!abi || abi.length === 0) {
      return <p className="f8">No Events found...</p>;
    }
    return (<div>
      <form className="mb4">
        <fieldset id="events" className={`bn pa0 ml0 ${listenToAllEvents ? 'o-10 pointer-none' : ''}`}>
          <p className="f8 lh-copy mb2">Select contract events:</p>
          <div style={{ maxHeight: '200px', overflowY: 'auto' }}>
            {
              abi
                .filter(topics => topics.type === 'event')
                .map(event => {
                  return (<Checkbox
                    label={event.name}
                    key={event.name}
                    toggle={() => this.toggleFromEvents(event.name)}
                    isActive={selectedEvents.some(eventName => eventName === event.name)}/>)
                })
            }
          </div>
        </fieldset>
      </form>
      <Checkbox
        label={'Listen to all events'}
        toggle={() => this.toggleEventListDisabled()}
        isActive={listenToAllEvents}/>
    </div>);
  }

  toggleEventListDisabled() {
    this.setState({ listenToAllEvents: !this.state.listenToAllEvents }, this.toggleEventChanged);
  }

  selectAllEvents() {
    const { abi } = this.props;
    this.setState({
      selectedEvents: abi.filter(topics => topics.type === 'event').map(event => event.name)
    }, this.toggleEventChanged);
  }

  toggleFromEvents(eventName) {
    const { selectedEvents } = this.state;
    if (selectedEvents.some(event => event === eventName)) {
      this.remove(selectedEvents, eventName);
    } else {
      this.add(selectedEvents, eventName);
    }
  }

  add(selectedEvents, eventName) {
    this.setState({
      selectedEvents: [...selectedEvents, eventName],
    }, this.toggleEventChanged);
  }

  remove(selectedEvents, eventName) {
    this.setState({
      selectedEvents: selectedEvents.filter(event => event !== eventName),
    }, this.toggleEventChanged);
  }

  toggleEventChanged() {
    const { abi, onEventsChanged } = this.props;
    const { selectedEvents, listenToAllEvents } = this.state;

    if (onEventsChanged) {
      if(listenToAllEvents) {
        onEventsChanged([]);
      } else {
        const structuredEvents = selectedEvents
          .map(eventName => getEventStructureByName(abi, eventName));
        onEventsChanged(structuredEvents);
      }
    }
  }
}
