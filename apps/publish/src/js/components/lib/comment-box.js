import React, { Component } from 'react';
import classnames from 'classnames';
import { Sigil } from '/components/lib/icons/sigil';

export class CommentBox extends Component {
  constructor(props){
    super(props);
  }

  componentDidUpdate(prevProps, prevState) {
    if (!prevProps.enabled && this.props.enabled) {
      if (this.textarea) {
        this.textarea.value = '';
      }
    }
  }

  render() {
    if (this.props.enabled) {
      return (
        <div className="cb w-100 flex" 
          style={{paddingBottom: 8, marginTop: 32}}>
          <div className="fl" style={{marginRight: 10}}>
            <Sigil ship={this.props.our} size={36}/>
          </div>
          <div className="flex-col w-100">
            <textarea className="body-regular-400 w-100"
              ref={(el) => {this.textarea = el}}
              style={{resize: "none"}}
              type="text"
              name="commentBody"
              defaultValue=''
              onChange={this.props.action}>
            </textarea>
            <p className="body-regular pointer" onClick={this.props.post}>
              -> Post
            </p>
          </div>
        </div>
      );
    } else {
      return (
        <div className="cb w-100 flex" 
          style={{paddingBottom: 8, marginTop: 32}}>
          <div className="fl" style={{marginRight: 10}}>
            <Sigil ship={this.props.our} size={36}/>
          </div>
          <div className="flex-col w-100">
            <textarea className="body-regular-400 w-100"
              ref={(el) => {this.textarea = el}}
              style={{resize: "none"}}
              type="text"
              name="commentBody"
              disabled={true}>
            </textarea>
            <p className="body-regular gray-50">
              -> Post
            </p>
          </div>
        </div>
      );
    }
  }
}
