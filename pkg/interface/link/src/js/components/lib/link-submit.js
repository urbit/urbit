import React, { Component } from 'react'
import { api } from '../../api';


export class LinkSubmit extends Component {
  constructor() {
    super();
    this.state = {
      linkValue: "",
      linkTitle: "",
      linkValid: false
    }
    this.setLinkValue = this.setLinkValue.bind(this);
    this.setLinkTitle = this.setLinkTitle.bind(this);
  }

  onClickPost() {
    let link = this.state.linkValue;
    let title = (this.state.linkTitle)
    ? this.state.linkTitle
    : this.state.linkValue;
    let request = api.postLink(this.props.path, link, title);

    if (request) {
      this.setState({linkValue: "", linkTitle: ""})
    }
  }

  setLinkValid(link) {
    let URLparser = new RegExp(/((?:([\w\d\.-]+)\:\/\/?){1}(?:(www)\.?){0,1}(((?:[\w\d-]+\.)*)([\w\d-]+\.[\w\d]+))){1}(?:\:(\d+)){0,1}((\/(?:(?:[^\/\s\?]+\/)*))(?:([^\?\/\s#]+?(?:.[^\?\s]+){0,1}){0,1}(?:\?([^\s#]+)){0,1})){0,1}(?:#([^#\s]+)){0,1}/);

    let validURL = URLparser.exec(link);

    if (!validURL) {
      let checkProtocol = URLparser.exec("http://" + link);
      if (checkProtocol) {
        this.setState({linkValid: true});
        this.setState({linkValue: "http://" + link});
      } else {
        this.setState({linkValid: false})
      }
    } else if (validURL) {
      this.setState({linkValid: true});
    }
  }

  setLinkValue(event) {
    this.setState({linkValue: event.target.value});
    this.setLinkValid(event.target.value);
  }

  setLinkTitle(event) {
    this.setState({linkTitle: event.target.value});
  }

  render() {

    let activeClasses = (this.state.linkValid)
    ? "green2 pointer"
    : "gray2";
    
    return (
      <div className="relative ba b--gray4 b--gray2-d br1 w-100 mb6">
        <textarea
        className="pl2 bg-gray0-d white-d w-100 f8"
        style={{
          resize: "none",
          height: 40,
          paddingTop: 10
        }}
        placeholder="Paste link here"
        onChange={this.setLinkValue}
        spellCheck="false"
        rows={1}
        onKeyPress={e => {
          if (e.key === "Enter") {
            e.preventDefault();
            this.onClickPost();
          }
        }}
        value={this.state.linkValue}
        />
        <textarea
        className="pl2 bg-gray0-d white-d w-100 f8"
        style={{
          resize: "none",
          height: 40,
          paddingTop: 16
        }}
        placeholder="Enter title"
        onChange={this.setLinkTitle}
        spellCheck="false"
        rows={1}
        onKeyPress={e => {
          if (e.key === "Enter") {
            e.preventDefault();
            this.onClickPost();
          }
        }}
        value={this.state.linkTitle}
        />
        <button
          className={"absolute bg-gray0-d f8 ml2 flex-shrink-0 " + activeClasses}
          disabled={!this.state.linkValid}
          onClick={this.onClickPost.bind(this)}
          style={{
            bottom: 12,
            right: 8
          }}>
            Post
        </button>
      </div>
    )
  }
}

export default LinkSubmit;
