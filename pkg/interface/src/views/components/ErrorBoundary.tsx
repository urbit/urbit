import React, { Component } from 'react';
import { RouteComponentProps, withRouter } from 'react-router-dom';
import StackTrace from 'stacktrace-js';
import { Spinner } from '~/views/components/Spinner';
import ErrorComponent from './Error';

class ErrorBoundary extends Component<
  RouteComponentProps,
  { error?: Error | true}
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

  componentDidCatch(error: Error) {
    this.setState({ error: true });
    StackTrace.fromError(error).then((stackframes) => {
      const stack = stackframes.map((frame) => {
        return `${frame.functionName} (${frame.fileName} ${frame.lineNumber}:${frame.columnNumber})`;
      }).join('\n');
      error = { name: error.name, message: error.message, stack };
      this.setState({ error });
    });
    return false;
  }

  render() {
    if (this.state.error) {
      if (this.state.error === true) {
        return (
          <div className="relative h-100 w-100">
            <Spinner
              text="Encountered error, gathering information"
              awaiting={true}
              classes="absolute top-0 bottom-0 left-0 right-0 flex items-center justify-center"
            />
          </div>
        );
      }
      return (<ErrorComponent error={this.state.error} />);
    }
    return this.props.children;
  }
}

export default withRouter(ErrorBoundary);
