import React from 'react';

class EndGame extends React.Component {

    render() {
        return (
            <div className='gameOverPanel'>
                <div className='gameOverLab'>Game Over</div>
                <button
                    className="textBtn"
                    onClick={() => window.location.reload()}
                >Restart</button>
            </div>
        );
    }
}

export default EndGame;