import React, { Component } from 'react';
import classnames from 'classnames';
import { Sigil } from '/components/lib/icons/sigil';

class PostButton extends Component {
  render() {
    if (this.props.enabled) {
      return (
        <p className="body-regular pointer" onClick={this.props.post}>
          -> Post
        </p>
      );
    } else {
      return (
        <p className="body-regular gray-30">
          -> Post
        </p>
      );
    }
  }
}

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
    let textClass = (this.props.enabled)
      ?  "body-regular-400 w-100"
      :  "body-regular-400 w-100 gray-30";
    return (
      <div className="cb w-100 flex" 
        style={{paddingBottom: 8, marginTop: 32}}>
        <div className="fl" style={{marginRight: 10}}>
          <Sigil ship={this.props.our} size={36}/>
        </div>
        <div className="flex-col w-100">
          <textarea className={textClass}
            ref={(el) => {this.textarea = el}}
            style={{resize: "none"}}
            type="text"
            name="commentBody"
            defaultValue=''
            onChange={this.props.action}
            disabled={(!this.props.enabled)}>
          </textarea>
          <PostButton 
            post={this.props.post} 
            enabled={(Boolean(this.props.content) && this.props.enabled)}
          /> 
        </div>
      </div>
    );
  }
}
