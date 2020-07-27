import React, { Component } from 'react';
import { RouteComponentProps, withRouter } from 'react-router-dom';

type ErrorProps = RouteComponentProps & {
  code?: number | string,
  description?: string,
  error?: Error
};

class ErrorComponent extends Component<ErrorProps> {
  render () {
    const { code, error, history, description } = this.props;
    return (
        <div className="pa4 inter tc flex flex-column items-center justify-center w-100 h-100">
          <h1 className="mb4 fw2 f2" style={{
            fontFeatureSettings: '\'zero\' 1',
          }}>
            {code ? code : 'Error'}
           </h1>
           {description ? <p className="tc mb4">{description}</p> : null}
          {error ? (
            <div className="mb4">
              <p className="mb4"><code>&ldquo;{error.message}&rdquo;</code></p>
              <details>
                <summary>Stack trace</summary>
                <pre className="tl">{error.stack}</pre>
              </details>
            </div>
            ) : null}
          <p className="tc mb4">If this is unexpected, email <code>support@tlon.io</code> or <a className="bb" href="https://github.com/urbit/urbit/issues/new/choose">submit an issue</a>.</p>
          {history.length > 1
            ? <button className="bg-light-green green2 pa2 pointer" onClick={() => history.go(-1) }>Go back</button>
            : <button className="bg-light-green green2 pa2 pointer" onClick={() => history.push('/') }>Go home</button>
          }
        </div>
      );
  }
}

export default withRouter(ErrorComponent);
