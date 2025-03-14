
from sklearn.linear_model import LinearRegression
import numpy as np

import matplotlib.pyplot as plt


def scatterplot(ax, xdata, ydata, color=None, addtext=None, yaxis=False, lims=[0, 45], cmap='YlGnBu'):
    """
    Plot fancy scatter plot with a linear regression
    """
    xdata = xdata.flatten()
    ydata = ydata.flatten()
    mask = np.where(~np.isnan(xdata) & ~np.isnan(ydata))
    xdata = xdata[mask]
    ydata = ydata[mask]
    bias = np.round(np.mean(ydata - xdata), 3)
    rmse = np.round(np.sqrt(np.mean((ydata - xdata)**2)), 3)
    x = xdata.reshape((-1, 1))
    y = ydata
    reg = LinearRegression().fit(x, y)
    z = reg.predict(x)
    r2 = np.round(reg.score(x, y), 3)

    if color is None:
        ax.scatter(xdata, ydata, marker='+')  # scatterplot xdata vs estimation
    else:
        color = color.flatten()[mask]
        sc = ax.scatter(xdata, ydata, c=color, cmap=cmap, marker='+')
        plt.colorbar(sc)

    # Add linear regression line
    ax.plot(x, z, color='blue', linewidth=1.5,
            label=f'Slope={reg.coef_[0]:.3f}, Intercept={reg.intercept_:.3f}\nR²={r2:.4}, bias={bias}, rmse={rmse}')
    ax.legend()

    if addtext is not None:
        addtext = addtext[mask]
        for idx, text in enumerate(addtext):
            ax.text(xdata[idx], ydata[idx], int(text))

    # Add x/y mean lines for article figures
    xmean = np.mean(x)
    ymean = np.mean(y)
    ax.plot([xmean, xmean], lims, color='red', linestyle='--', linewidth=1.5)
    ax.plot(lims, [ymean, ymean], color='red', linestyle='--', linewidth=1.5)

    # Plot bissectrice and adjuste axes limits
    ax.plot(lims, lims, 'k-', alpha=0.75, zorder=0, linewidth=1.5)
    ax.set_aspect('equal')
    ax.set_xlim(lims)
    ax.set_ylim(lims)
    if yaxis:
        ax.set_yticks(np.linspace(lims[0], lims[1], 5))
        ax.tick_params(
            axis='y',
            labelsize=14)
