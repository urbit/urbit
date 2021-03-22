import React, { Component } from 'react';
import { RouteComponentProps, withRouter } from 'react-router-dom';
import ErrorComponent from './Error';

class ErrorBoundary extends Component<
  RouteComponentProps,
  { error?: Error }
> {
  constructor(props) {
    super(props);
    this.state = { error: undefined };
    const { history } = this.props;
    history.listen((location, action) => {
      if (this.state.error) {
        this.setState({
          error: undefined
        });
      }
    });
  }

  componentDidCatch(error) {
    this.setState({ error });
    return false;
  }

  render() {
    if (this.state.error) {
      return (<ErrorComponent error={this.state.error} />);
    }
    return this.props.children;
  }
}

export default withRouter(ErrorBoundary);
