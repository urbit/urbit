import React, { Component } from 'react';
import { Link } from 'react-router-dom';

export class Dropdown extends Component {
  constructor(props) {
    super(props);

    this.toggleDropdown = this.toggleDropdown.bind(this);
    this.handleClickOutside = this.handleClickOutside.bind(this);
    this.collapseAndDispatch = this.collapseAndDispatch.bind(this);
    this.state = {
      open: false
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
      this.setState({open: false});
    }
  }

  toggleDropdown() {
    this.setState({open: !this.state.open});
  }

  collapseAndDispatch(action){
    this.setState({open: false}, action);
  }

  render() {
    let alignment = (!!this.props.align)
      ? this.props.align : "right";

    let display = (this.state.open)
      ? "block" : "none";

    let optionsClass = (this.state.open)
      ? "open" : "closed";

    let leftAlign = "";
    let rightAlign = "0";

    if (alignment === "left") {
      leftAlign = "0"
      rightAlign = ""
    }

    let optionsList = this.props.options.map((val, i) => {
      return (
        <button key={i} className={val.cls}
          onClick={() => this.collapseAndDispatch(val.action)}>
          {val.txt}
        </button>
      );
    });

    return (
      <div className={"options relative dib pr3 pointer " + optionsClass}
        ref={(el) => {this.optsButton = el}}
        onClick={this.toggleDropdown}>
        <button className="bg-transparent white-d pointer mb1 br2 pa2 pr4">
          {this.props.buttonText}
        </button>
        <div className="absolute flex flex-column pv2 ba b--gray4 br2 z-1 bg-white bg-gray0-d"
           ref={(el) => {this.optsList = el}}
          style={{left: leftAlign, right: rightAlign, width:this.props.width, display: display}}>
          {optionsList}
        </div>
      </div>
    )
  }
}

export default Dropdown
