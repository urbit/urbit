import React, { Component } from 'react';

export class LinkDetail extends Component {

  constructor(props) {
    super(props);
    this.state = {
      comment: '',
      data: props.data,
      disabled: false
    };
  }

  onClickPost() {
    const url = this.props.url || '';
    // TODO: use graph API
    this.props.api.links.postComment(
      this.props.resourcePath,
      url,
      this.state.comment
    ).then(() => {
      this.setState({ comment: '', disabled: false });
    });
  }

  render() {
    const activeClasses = this.state.comment
      ? 'black white-d pointer'
      : 'gray2 b--gray2';


    return (
      <div className="relative">
        <div className='relative ba br1 mt6 mb6'>
          <textarea
            className="w-100 bg-gray0-d white-d f8 pa2 pr8"
            style={{
              resize: 'none',
              height: 75
            }}
            placeholder="Leave a comment on this link"
            onChange={this.setComment}
            value={this.state.comment}
          />
          <button
            className={
              'f8 bg-gray0-d ml2 absolute ' + activeClasses
            }
            disabled={!this.state.comment || this.state.disabled}
            onClick={this.onClickPost.bind(this)}
            style={{
              bottom: 12,
              right: 8
            }}>
            Post
          </button>
        </div>
        <Spinner
          awaiting={this.state.disabled}
          classes="absolute pt5 right-0"
          text="Posting comment..." />
      </div>
    );
  }
}

