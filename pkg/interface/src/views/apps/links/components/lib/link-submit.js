import React, { Component } from 'react';
import { Spinner } from '~/views/components/Spinner';
import { createPost } from '~/logic/api/graph';


export class LinkSubmit extends Component {
  constructor() {
    super();
    this.state = {
      linkValue: '',
      linkTitle: '',
      submitFocus: false,
      disabled: false
    };
    this.setLinkValue = this.setLinkValue.bind(this);
    this.setLinkTitle = this.setLinkTitle.bind(this);
  }

  onClickPost() {
    const link = this.state.linkValue;
    const title = this.state.linkTitle
      ? this.state.linkTitle
      : this.state.linkValue;

    const parentIndex = this.props.parentIndex || '';
    let post = createPost([
      { text: title },
      { url: link }
    ], parentIndex);

    this.setState({ disabled: true }, () => {
      this.props.api.graph.addPost(
        `~${this.props.ship}`,
        this.props.name,
        post
      ).then((r) => {
        this.setState({
          disabled: false,
          linkValue: '',
          linkTitle: '',
        });
      });
    });
  }

  setLinkValue(event) {
    this.setState({ linkValue: event.target.value });
  }

  setLinkTitle(event) {
    this.setState({ linkTitle: event.target.value });
  }

  render() {
    const activeClasses = (!this.state.disabled) ? 'green2 pointer' : 'gray2';

    const focus = (this.state.submitFocus)
      ? 'b--black b--white-d'
      : 'b--gray4 b--gray2-d';

    return (
      <div className={'relative ba br1 w-100 mb6 ' + focus}>
        <textarea
          className="pl2 bg-gray0-d white-d w-100 f8"
          style={{
            resize: 'none',
            height: 40,
            paddingTop: 10
          }}
          placeholder="Paste link here"
          onChange={this.setLinkValue}
          onBlur={() => this.setState({ submitFocus: false })}
          onFocus={() => this.setState({ submitFocus: true })}
          spellCheck="false"
          rows={1}
          onKeyPress={(e) => {
            if (e.key === 'Enter') {
              e.preventDefault();
              this.onClickPost();
            }
          }}
          value={this.state.linkValue}
        />
        <textarea
          className="pl2 bg-gray0-d white-d w-100 f8"
          style={{
            resize: 'none',
            height: 40,
            paddingTop: 16
          }}
          placeholder="Enter title"
          onChange={this.setLinkTitle}
          onBlur={() => this.setState({ submitFocus: false })}
          onFocus={() => this.setState({ submitFocus: true })}
          spellCheck="false"
          rows={1}
          onKeyPress={(e) => {
            if (e.key === 'Enter') {
              e.preventDefault();
              this.onClickPost();
            }
          }}
          value={this.state.linkTitle}
        />
        <button
          className={
            'absolute bg-gray0-d f8 ml2 flex-shrink-0 ' + activeClasses
          }
          disabled={this.state.disabled}
          onClick={this.onClickPost.bind(this)}
          style={{
            bottom: 12,
            right: 8
          }}
        >
          Post
        </button>
        <Spinner
          awaiting={this.state.disabled}
          classes="mt3 absolute right-0"
          text="Posting to collection..." />
      </div>
    );
  }
}

