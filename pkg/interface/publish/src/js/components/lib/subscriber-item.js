import React, { Component } from 'react';
import { Link } from 'react-router-dom';

export class SubscriberItem extends Component {
  constructor(props) {
    super(props);

    this.toggleOptions = this.toggleOptions.bind(this);
    this.handleClickOutside = this.handleClickOutside.bind(this);
    this.ban = this.ban.bind(this);
    this.unban = this.unban.bind(this);
    this.removeWriter = this.removeWriter.bind(this);
    this.addWriter = this.addWriter.bind(this);
    this.state = {
      optionsSelected: false
    }
  }

  componentDidMount() {
    document.addEventListener('mousedown', this.handleClickOutside);
  }

  componentWillUnmount() {
    document.removeEventListener('mousedown', this.handleClickOutside);
  }

  handleClickOutside(evt) {
    if (this.optsList && !this.optsList.contains(evt.target) &&
        this.optsButton && !this.optsButton.contains(evt.target)) {
      this.setState({optionsSelected: false});
    }
  }

  ban() {
    let action = {
      add: {
        members: [this.props.who],
        path: this.props.readPath,
      }
    }
    this.setState({optionsSelected: false}, () => {
      window.api.action("group-store", "group-action", action);
    });
  }

  unban() {
    let action = {
      remove: {
        members: [this.props.who],
        path: this.props.readPath,
      }
    }
    this.setState({optionsSelected: false}, () => {
      window.api.action("group-store", "group-action", action);
    });
  }

  removeWriter() {
    let action = {
      remove: {
        members: [this.props.who],
        path: this.props.writePath,
      }
    }
    this.setState({optionsSelected: false}, () => {
      window.api.action("group-store", "group-action", action);
    });
  }

  addWriter() {
    let action = {
      add: {
        members: [this.props.who],
        path: this.props.writePath,
      }
    }
    this.setState({optionsSelected: false}, () => {
      window.api.action("group-store", "group-action", action);
    });
  }

  toggleOptions() {
    this.setState({optionsSelected: !this.state.optionsSelected});
  }

  render() {
    let display = (this.state.optionsSelected)
      ? "block" : "none";

    let width = 0;
    let options = [];
    if (this.props.section === 'participants') {
      if (this.props.readPath === this.props.writePath) {
        width = 258;
        let url = `/~contacts${this.props.writePath}`;
        options = [
          <a key={0} className="tl pointer" href={url}>
            Manage this group in the contacts view
          </a>
        ];
      } else {
        width = 157;
        options = [
          <button key={0} className="tl pointer" onClick={this.removeWriter}>
            Demote to subscriber
          </button>
        ];
      }
    } else if (this.props.section === 'subscribers') {
      width = 162;
      options = [
        <button key={0} className="tl mb2 pointer" onClick={this.addWriter}>
          Promote to participant
        </button>,
        <button key={1} className="tl red2 pointer" onClick={this.ban}>
          Ban
        </button>
      ];
    } else if (this.props.section === 'banned') {
      width = 72;
      options = [
        <button key={0} className="tl red2 pointer" onClick={this.unban}>
          Unban
        </button>
      ];
    }

    let optionsColor = (this.state.optionsSelected)
      ? '#e6e6e6' : 'white';

    return (
      <div className="flex justify-between">
        <div className="f9 mono mr2">{this.props.who}</div>
        <div className="options relative dib"
             ref={(el) => {this.optsButton = el}}>
          <button className="pr3 mb1 pointer br2 pa2 pr4"
                  style={{backgroundColor: optionsColor}}
                  onClick={this.toggleOptions}>
            Options
          </button>
          <div className="absolute flex flex-column pa4 ba b--gray4 br2 z-1 bg-white"
             ref={(el) => {this.optsList = el}}
              style={{right:0, width:width, display: display}}>
            {options}
          </div>
        </div>
      </div>
    )
  }
}

export default SubscriberItem
